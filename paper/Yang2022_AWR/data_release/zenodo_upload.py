"""Create (or reuse) a Zenodo DRAFT for the LRM SWE dataset and upload the files. Never publishes.

Usage: python zenodo_upload.py UPLOAD_DIR [--deposit ID]
Token: read from ~/.zenodo_token (scopes deposit:write, deposit:actions).
"""
import argparse
import os
import sys

import requests

API = "https://zenodo.org/api"

DESCRIPTION = """
<p>Daily snow water equivalent (SWE) for the Sierra Nevada, California, at ~500 m resolution for water years
2001&ndash;2019 (4,295 model days), from the two linear regression models (LRM) evaluated in
Yang et al. (2022, <i>Advances in Water Resources</i> 160, 104075):</p>
<ul>
<li><b>LRM-FSCA</b>: physiographic variables, a historical SWE pattern from the Sierra Nevada Snow Reanalysis,
and MODIS daily mean fractional snow-covered area. This is the recommended product.</li>
<li><b>LRM-baseline</b>: physiographic variables and the historical SWE pattern.</li>
</ul>
<p>Each day an elastic-net regression is trained on snow pillow SWE (~90&ndash;110 stations) and applied to
every grid cell.</p>
<p>Contents:</p>
<ul>
<li>daily SWE grids as NetCDF-4/CF, one file per water year and model</li>
<li>the daily station training data</li>
<li>regression coefficients</li>
<li>Monte Carlo cross-validation statistics</li>
<li>the inputs needed to redraw the paper's figures</li>
</ul>
<p>Negative regression estimates were set to 0. See README.md for details.</p>
<p>Code: https://github.com/KehanGit/StationSWERegressionV2</p>
<p>Funding: NASA grants NNX17AF50G, 80NSSC17K0071, 80NSSC18K0427 P00001; NOAA NA18OAR4590367; Niwot Ridge LTER
(NSF DEB 1637686); NOAA RISA Western Water Assessment.</p>
""".strip()

METADATA = {
    "upload_type": "dataset",
    "title": "Daily 500 m snow water equivalent for the Sierra Nevada from a linear regression model (LRM), "
             "water years 2001-2019",
    "creators": [{"name": "Yang, Kehan", "orcid": "0000-0003-0319-4739"}],
    "description": DESCRIPTION,
    "access_right": "open",
    "license": "cc-by-4.0",
    "keywords": ["snow water equivalent", "SWE", "Sierra Nevada", "snow pillow", "MODIS",
                 "fractional snow-covered area", "linear regression", "elastic net", "snow hydrology"],
    "related_identifiers": [
        {"identifier": "10.1016/j.advwatres.2021.104075", "relation": "isSupplementTo", "scheme": "doi",
         "resource_type": "publication-article"},
        {"identifier": "https://github.com/KehanGit/StationSWERegressionV2", "relation": "isSupplementTo",
         "scheme": "url", "resource_type": "software"},
    ],
    "version": "1.0",
    "language": "eng",
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("upload_dir")
    ap.add_argument("--deposit", help="existing draft id to reuse")
    a = ap.parse_args()
    token = open(os.path.expanduser("~/.zenodo_token")).read().strip()
    s = requests.Session()
    s.headers["Authorization"] = f"Bearer {token}"

    if a.deposit:
        dep = s.get(f"{API}/deposit/depositions/{a.deposit}").json()
    else:
        r = s.post(f"{API}/deposit/depositions", json={})
        r.raise_for_status()
        dep = r.json()
    dep_id, bucket = dep["id"], dep["links"]["bucket"]
    r = s.put(f"{API}/deposit/depositions/{dep_id}", json={"metadata": {**METADATA, "prereserve_doi": True}})
    if not r.ok:
        sys.exit(f"metadata failed: {r.status_code} {r.text[:500]}")
    doi = r.json()["metadata"].get("prereserve_doi", {}).get("doi")
    print(f"draft {dep_id}, reserved DOI {doi}", flush=True)

    existing = {f["filename"]: f["filesize"] for f in s.get(f"{API}/deposit/depositions/{dep_id}/files").json()}
    files = sorted(os.listdir(a.upload_dir))
    for i, fn in enumerate(files, 1):
        p = os.path.join(a.upload_dir, fn)
        if fn.startswith(".") or not os.path.isfile(p):
            continue
        size = os.path.getsize(p)
        if existing.get(fn) == size:
            print(f"[{i}/{len(files)}] {fn}: already uploaded", flush=True)
            continue
        for attempt in range(3):
            with open(p, "rb") as fh:
                r = s.put(f"{bucket}/{fn}", data=fh, timeout=3600)
            if r.ok:
                break
            print(f"  retry {fn}: {r.status_code}", flush=True)
        if not r.ok:
            sys.exit(f"upload failed: {fn} {r.status_code} {r.text[:300]}")
        print(f"[{i}/{len(files)}] {fn}: {size / 1e6:.1f} MB ok", flush=True)
    print(f"DONE draft {dep_id} DOI {doi} — review at https://zenodo.org/uploads/{dep_id}", flush=True)


if __name__ == "__main__":
    main()
