# Yang et al. (2022), Advances in Water Resources — code

> Yang, K., Musselman, K. N., Rittger, K., Margulis, S. A., Painter, T. H., & Molotch, N. P. (2022).
> Combining ground-based and remotely sensed snow data in a linear regression model for real-time
> estimation of snow water equivalent. *Advances in Water Resources*, 160, 104075.
> https://doi.org/10.1016/j.advwatres.2021.104075

Output data (daily SWE for both models, training data, coefficients, cross-validation statistics):
Zenodo, https://doi.org/10.5281/zenodo.24547693

## Layout

| Path | What |
|---|---|
| `config.R` | every input/output path and model setting |
| `01_run_LRM.R` | daily model runs (LRM-FSCA, LRM-baseline) |
| `functions/` | the model functions as used in the paper (differ from the package `R/` versions) |
| `figures/` | one script per paper figure (Figs. 3–11); `fig_config.R` holds their paths |
| `data_release/make_zenodo_package.py` | builds the Zenodo dataset from the run outputs |
| `data_release/make_figure_data.sh` | copies the figure inputs into the dataset's `figure_data/` |
| `_original/` | the scripts exactly as run in 2020–2021, incl. figure scripts (see its README for the figure → script map and run history) |

## Running the model

```sh
cd paper/Yang2022_AWR
export LRM_INPUT=/path/to/P2_LRM_test LRM_DATA=/path/to/dataset LRM_OUTPUT=/path/to/output
Rscript 01_run_LRM.R LRM-FSCA main                       # all WY2001–2019 dates
Rscript 01_run_LRM.R LRM-FSCA gapfill                    # June 2020 gap-fill dates
Rscript 01_run_LRM.R LRM-baseline main --dates 2017-04-01 --cores 4
```

R packages: raster, sp, dplyr, tidyr, tibble, purrr, readr, modelr, glmnet, glmnetUtils, hydroGOF
(and sf only if an FSCA day has to be resampled). Tested with R 4.4.2 in 2026.

Inputs (see `config.R`):
- daily CDEC snow pillow SWE (`station_20200419.txt`)
- 18 physiographic grids (`input/res500/phv`)
- MODIS fractional snow-covered area and its daily mean (DMFSCA)
- the Sierra Nevada snow reanalysis (historical SWE pattern)
- the historical-day tables.

The released dataset combines:
- the `main` runs
- the `gapfill` runs, which replace or add days
- a 2011-01-01 rerun that used the 2010-12-31 FSCA.

See `_original/README.md` for the full run history.

## Figures

```sh
cd paper/Yang2022_AWR/figures
export LRM_P2_ROOT=/path/to/zenodo/figure_data LRM_SHAPE_DIR=/path/to/zenodo/figure_data/shapes
Rscript fig05_snow_course_validation.R        # writes out/fig05_snow_course_validation.jpg
```

| Figure | Script | Notes |
|---|---|---|
| 1, 2 | — | study-area map and method diagram (made outside R) |
| 3 | `fig03_snow_pillow_elevation.R` | |
| 4 | `fig04_cross_validation.R` | rebuilds the QC table (Jan–Jun, > 40 stations) from per-run statistics |
| 5 | `fig05_snow_course_validation.R` | 6,592 snow-course pairs |
| 6 | `fig06_ASO_validation.R` | 23 ASO dates (see note in script) |
| 7 | `fig07_ASO_error_maps.R` | |
| 8, 9 | `fig08_09_snow_storage.R` | |
| 10, 11 | `fig10_11_peak_swe.R` | |

R packages: ggplot2, dplyr, tidyr, raster, sf, ggpubr, cowplot, forcats, RColorBrewer, hydroGOF, outliers.

Every figure was re-drawn in 2026 and compared with the published version. The data and statistics match:
- Fig. 5: R², RMAE, RMSE and PBIAS match for all four products.
- Fig. 9: the median peak storage and peak dates match.
- Fig. 7: the error ranges match.

Layout and colours are close, but some panels of the published figures were assembled by hand. The
figure inputs are summaries made by the analysis scripts in `_original/` (extraction at snow courses,
comparison with ASO, Sierra-wide and pixel-wise statistics). They are published with the dataset in
`figure_data/`, so the figures can be redrawn without the original drives.

## Reproducibility

The model fit chooses alpha and lambda by k-fold cross-validation, and the 2020 runs set no random
seed. A 2026 re-run with this code (four test days, both models):
- **Training data:** reproduced byte-for-byte (`modeldata_*.txt`: stations, pillow SWE, FSCA, historical day, predictors).
- **SWE maps:** close to the released files but not identical (pixel correlation 0.95–0.999, domain-mean SWE within a few percent). The extent of valid pixels can also differ slightly, because which predictors are retained changes from fit to fit.

The cleaned code now fixes the seed (`SEED = 2022` in `config.R`, per day `SEED + YYYYMMDD`; override
with `LRM_SEED`). Re-runs are byte-identical regardless of core count; checked with 1 vs 2 cores. The
released files (unseeded 2020 runs) remain the reference.

Changes made to the 2020 code so it runs on current R (logic unchanged):
- `pickAlpha`: `as.list()` around two row assignments (current tibble)
- `setup_modeldata_oneday`: `tbl_df` → `as_tibble`; the model-data table is appended by path (current readr)
- package helpers that depend on archived CRAN packages (`gdalUtils`) are replaced in `functions/lrm_helpers.R`.
