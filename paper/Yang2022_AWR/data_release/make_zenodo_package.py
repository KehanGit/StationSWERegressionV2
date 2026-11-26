"""Package the final LRM SWE runs for Zenodo (Yang et al. 2022, AWR).

Input  (per model): daily GeoTIFFs phvrcn_YYYYMMDD.tif (m, nodata -99), modeldata_YYYYMMDD.txt,
                    phvrcn_coefs_YYYYMMDD.txt, crossval_stats_dates/phvrcn_<metric>_YYYYMMDD.txt
Output (per model): swe/<MODEL>/<MODEL>_SWE_WY<YYYY>.nc  (int16 mm, scale 0.001 -> m)
                    tables/<MODEL>_training_data.csv.gz, _coefficients.csv.gz, _crossval_stats.csv.gz

Processing: water years 2001-2019 only; negative SWE set to 0; nodata -> missing.

Usage: python make_zenodo_package.py SRC_DIR OUT_DIR [--wy 2001 2019] [--models LRM-FSCA LRM-baseline]
"""
import argparse
import datetime as dt
import glob
import gzip
import io
import os
import re

import numpy as np
import pandas as pd
import rasterio
import xarray as xr

RUNS = {"LRM-FSCA": "LRM-FSCA_v1", "LRM-baseline": "LRM-baseline_v1"}
DESCRIPTION = {
    "LRM-FSCA": "linear regression model with physiographic predictors, historical SWE pattern "
                "and MODIS daily mean fractional snow-covered area (DMFSCA)",
    "LRM-baseline": "linear regression model with physiographic predictors and historical SWE pattern",
}
PAPER = ("Yang, K., Musselman, K.N., Rittger, K., Margulis, S.A., Painter, T.H., Molotch, N.P. (2022). "
         "Combining ground-based and remotely sensed snow data in a linear regression model for real-time "
         "estimation of snow water equivalent. Advances in Water Resources, 160, 104075. "
         "https://doi.org/10.1016/j.advwatres.2021.104075")
METHOD = ("Schneider, D. and Molotch, N.P. (2016). Real-time estimation of snow water equivalent in the Upper "
          "Colorado River Basin using MODIS-based SWE Reconstructions and SNOTEL data. Water Resources "
          "Research, 52(10), 7892-7910. https://doi.org/10.1002/2016WR019067")
STD_SHAPE = (1584, 1368)  # rows, cols of the Sierra 500 m grid


def wy_of(d):
    return d.year + 1 if d.month >= 10 else d.year


def dates_in(run_dir, wy0, wy1):
    out = []
    for f in glob.glob(os.path.join(run_dir, "phvrcn_2*.tif")):
        m = re.search(r"phvrcn_(\d{8})\.tif$", f)
        if m and not os.path.basename(f).startswith("._"):
            d = dt.datetime.strptime(m.group(1), "%Y%m%d").date()
            if wy0 <= wy_of(d) <= wy1:
                out.append(d)
    return sorted(out)


def grid_coords(path):
    with rasterio.open(path) as r:
        t = r.transform
        lon = t.c + t.a * (np.arange(r.width) + 0.5)
        lat = t.f + t.e * (np.arange(r.height) + 0.5)
    return lat, lon


def write_wy(model, run_dir, wy, dates, out_dir, lat, lon):
    arr = np.full((len(dates), len(lat), len(lon)), np.nan, dtype=np.float32)
    n_neg = 0
    for i, d in enumerate(dates):
        with rasterio.open(os.path.join(run_dir, f"phvrcn_{d:%Y%m%d}.tif")) as r:
            a = np.asarray(r.read(1), dtype="f8")
        if a.shape != STD_SHAPE:
            raise ValueError(f"{model} {d}: unexpected grid {a.shape}")
        valid = np.isfinite(a) & (a > -90)
        n_neg += int((a[valid] < 0).sum())
        a = np.clip(a, 0, None)
        arr[i][valid] = a[valid]
    ds = xr.Dataset(
        {"swe": (("time", "lat", "lon"), arr)},
        coords={"time": pd.to_datetime(dates), "lat": lat, "lon": lon},
    )
    ds["swe"].attrs = {
        "long_name": f"snow water equivalent ({model})", "standard_name": "lwe_thickness_of_surface_snow_amount",
        "units": "m", "grid_mapping": "crs",
        "comment": "Negative regression estimates were set to 0. Days without a model run are absent "
                   "(model is not run when too few snow pillows report snow).",
    }
    ds["crs"] = xr.DataArray(np.int32(0), attrs={
        "grid_mapping_name": "latitude_longitude", "epsg_code": "EPSG:4326",
        "semi_major_axis": 6378137.0, "inverse_flattening": 298.257223563})
    ds["lat"].attrs = {"standard_name": "latitude", "units": "degrees_north", "long_name": "pixel-center latitude"}
    ds["lon"].attrs = {"standard_name": "longitude", "units": "degrees_east", "long_name": "pixel-center longitude"}
    ds.attrs = {
        "title": f"Daily 500 m snow water equivalent for the Sierra Nevada, {model}, water year {wy}",
        "summary": f"Daily SWE from the {DESCRIPTION[model]}, trained each day on snow pillow SWE.",
        "institution": "University of Colorado Boulder, INSTAAR",
        "creator_name": "Kehan Yang",
        "references": PAPER + " | " + METHOD,
        "license": "CC-BY-4.0",
        "spatial_resolution": "0.0041667 degree (~500 m)",
        "water_year": f"{wy} (Oct 1 {wy - 1} - Sep 30 {wy})",
        "processing": "negative SWE set to 0; nodata (-99) stored as _FillValue; packed as int16 mm",
        "source_run": os.path.basename(run_dir.rstrip("/")),
        "Conventions": "CF-1.8",
        "date_created": dt.date.today().isoformat(),
    }
    enc = {"swe": {"dtype": "int16", "scale_factor": 0.001, "add_offset": 0.0, "_FillValue": -9999,
                   "zlib": True, "complevel": 5, "shuffle": True, "chunksizes": (1, len(lat), len(lon))},
           "time": {"units": "days since 2000-10-01", "calendar": "standard", "dtype": "int32"}}
    os.makedirs(os.path.join(out_dir, "swe", model), exist_ok=True)
    fn = os.path.join(out_dir, "swe", model, f"{model}_SWE_WY{wy}.nc")
    tmp = fn + ".part"
    ds.to_netcdf(tmp, encoding=enc, format="NETCDF4")
    os.replace(tmp, fn)
    return fn, n_neg


def read_commented_table(path):
    with open(path) as f:
        lines = f.readlines()
    header = [l[1:].strip() for l in lines if l.startswith("#")]
    body = "".join(l for l in lines if not l.startswith("#") and l.strip())
    return header, pd.read_csv(io.StringIO(body), sep="\t")


def write_tables(model, run_dir, dates, out_dir):
    os.makedirs(os.path.join(out_dir, "tables"), exist_ok=True)
    train, coefs, cv = [], [], []
    for d in dates:
        ds = f"{d:%Y%m%d}"
        mf = os.path.join(run_dir, f"modeldata_{ds}.txt")
        if os.path.exists(mf):
            _, t = read_commented_table(mf)
            t.insert(0, "date", d.isoformat())
            train.append(t)
        cf = os.path.join(run_dir, f"phvrcn_coefs_{ds}.txt")
        if os.path.exists(cf):
            _, c = read_commented_table(cf)  # historical SWE date is in training_data.rdate
            c.columns = [x.strip() for x in c.columns]
            c["predictor"] = c["predictor"].str.strip()
            c.insert(0, "date", d.isoformat())
            coefs.append(c)
        row = {"date": d.isoformat()}
        for m in ["r2", "rmse", "pctmae", "pbias"]:
            p = os.path.join(run_dir, "crossval_stats_dates", f"phvrcn_{m}_{ds}.txt")
            if os.path.exists(p):
                s = pd.read_csv(p, sep="\t", na_values=["NA", "NaN"])
                for col in s.columns[1:]:
                    row[col] = s[col].iloc[0]
        cv.append(row)
    for name, frames in [("training_data", train), ("coefficients", coefs)]:
        df = pd.concat(frames, ignore_index=True)
        if name == "training_data" and "rdate" in df:
            df["rdate"] = df["rdate"].astype(str).str.lstrip("X")
        df.to_csv(os.path.join(out_dir, "tables", f"{model}_{name}.csv.gz"), index=False, compression="gzip")
    pd.DataFrame(cv).to_csv(os.path.join(out_dir, "tables", f"{model}_crossval_stats.csv.gz"),
                            index=False, compression="gzip")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("src")
    ap.add_argument("out")
    ap.add_argument("--wy", nargs=2, type=int, default=[2001, 2019])
    ap.add_argument("--models", nargs="+", default=list(RUNS))
    ap.add_argument("--skip-tables", action="store_true")
    a = ap.parse_args()
    for model in a.models:
        run_dir = os.path.join(a.src, RUNS[model])
        dates = dates_in(run_dir, *a.wy)
        lat, lon = grid_coords(os.path.join(run_dir, f"phvrcn_{dates[0]:%Y%m%d}.tif"))
        print(f"{model}: {len(dates)} days, {dates[0]} .. {dates[-1]}", flush=True)
        for wy in range(a.wy[0], a.wy[1] + 1):
            wd = [d for d in dates if wy_of(d) == wy]
            if not wd:
                print(f"  WY{wy}: no days", flush=True)
                continue
            fn = os.path.join(a.out, "swe", model, f"{model}_SWE_WY{wy}.nc")
            if os.path.exists(fn):
                print(f"  WY{wy}: exists, skip", flush=True)
                continue
            fn, n_neg = write_wy(model, run_dir, wy, wd, a.out, lat, lon)
            print(f"  WY{wy}: {len(wd)} days, {n_neg} negative px set to 0, "
                  f"{os.path.getsize(fn) / 1e6:.0f} MB", flush=True)
        if not a.skip_tables:
            write_tables(model, run_dir, dates, a.out)
            print(f"  tables written", flush=True)


if __name__ == "__main__":
    main()
