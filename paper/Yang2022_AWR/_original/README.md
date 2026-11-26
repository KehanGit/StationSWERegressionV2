# Original analysis scripts (as run for the paper)

Unmodified copies of the scripts used for Yang et al. (2022), *Advances in Water
Resources* 160, 104075, https://doi.org/10.1016/j.advwatres.2021.104075.
Kept for provenance; cleaned, runnable versions live one level up.

Only change: a credential embedded in a commented-out install URL
(`pipeline/0_run_LRM.R`, line 15) was removed.

| Folder | Source (on the archive drive) | Notes |
|---|---|---|
| `pipeline/` | `rc_projects_20220416/P2_LRM_test/scripts/0Pipelining` | Final run `20210401_LRM_V1_pl_wfsca_norec` on CU Summit |
| `functions/` | `.../scripts/Model_funs`, `.../scripts/0functions` | Versions sourced by the paper runs — differ from the package `R/` |
| `plots/` | `GoogleDrive/GitHub/R/P2_LRM_test/{0review/plot,results,validation}` | Figure scripts (see table below) |
| `best_historical_day/` | `.../P2_LRM_test/{besthisday,0review}` | Selection of analog historical SWE day |
| `create_inputs/` | `.../scripts/create_inputs` | Physiographic predictors (barriers, distance to ocean) |

Paths point to the original machines (`/scratch/summit/keya6539`, `/projects/keya6539`, `D:/GoogleDrive`).

## Paper figure → script

Matched by output file name against `GoogleDrive/Research/P2_LRM/0submission/final_submission_20211121/figures`.

| Fig | Final file | Script | Match |
|---|---|---|---|
| 1 | `fig1_studyarea.jpg` | — (GIS map) | — |
| 2 | `fig2_LRM_method.jpg` | — (diagram) | — |
| 3 | `fig3_snow_pillow_elevational_dis.jpg` | `plots/7_snow_pillow_distribution.R` | file name |
| 4 | `fig4_snow_pillow_cv.jpg` | `plots/1_cross_validation_plot.R` (data: `pipeline/1_cross_validation.R`) | file name (`0_cv_all_*`) |
| 5 | `fig5_snow_course_validation.jpg` | `plots/2_validation_snow_course.R` | file name |
| 6 | `fig6_validation_ASO.jpg` | `plots/3_validation_ASO.R` | file name |
| 7 | `fig7_SWE_error_distribution.jpg` | `plots/3_1_validation_ASO_error_analysis.R` (rose plot) or `3_1_validation_ASO_error_maps.R` | to verify visually |
| 8 | `fig8_average_snow_storage_line_v2.jpg` | `plots/6_peakSWE_mean.R` (l. 80) | exact file name |
| 9 | `fig9_maximumSWS_ts.jpg` | `plots/6_peakSWE_mean.R` ("Maximum SWS (Gt)") | axis label |
| 10 | `fig10_ele_pixel_wise_peakSWE_20210128_v5_WM.jpg` | `plots/6_peakSWE_mean.R` (l. 434) | exact file name |
| 11 | `fig11_peak_snow_storage_maps3_20211121_new.jpg` | `plots/5_pixelwise_peaSWE_maps.R` / `6_peakSWE_mean.R` (l. 532) | file name |

Reviewer-response figures (GLM vs GLMNET, one- vs three-day): `plots/validation_*.R`.

## Final model runs (the released data)

The released SWE (`LRM-FSCA_v1`, `LRM-baseline_v1` = runs `20200509_LRM_V1_pl_wfsca_dm` /
`20200509_LRM_V1_pl_wofsca`) were produced with the scripts in `final_runs_2020/`, recovered from git
commit `30380c8` (2020-05-20) of `GoogleDrive/GitHub/R/P2_LRM_test/lrmtest/`:

| File | Original name | Model |
|---|---|---|
| `run_LRM-FSCA.R` | `run_snm_allv_newfsca_all_oneday_20200418_wfsca_dm.R` | PHV + historical SWE (`rcn`) + `vfscadm` |
| `run_LRM-baseline.R` | `run_snm_allv_newfsca_all_oneday_20200418_wofsca.R` | PHV + historical SWE (`rcn`) |
| `setup_modeldata_oneday.R` | `lrmtest/setup_modeldata_oneday.R` | data setup sourced by these runs |
| `monthly_data_sharing.R` | `datasharing/monthly_data_sharing.R` | monthly product shared with users |

Inputs: `hisdate_SS_num_r2.txt`, `simdate_all_wy_01_19.txt`, `station_20200419.txt`; cross-validation
`crossv_mc(n = 30, test = 0.1)` per day.

**Run history** (from the author's 2020 notes, `PHD_8T_1:Data/SWE/dataset/med/LRM/0readme.txt`, checked 2026-11-25):
- `20200418_*` — first run (these scripts' `RUNNAME`). `20200509_*` — second run, "new version of best
  historic days"; used in the paper. The two have byte-identical training tables (284/284 sampled days,
  same `rdate`) but different coefficients and maps (grid-mean SWE within ~±10%, r 0.96–0.996). Most
  likely cause: run-to-run randomness. `pickAlpha`/`cv.glmnet` assign CV folds at random and no seed was
  set. Re-running the cleaned `01_run_LRM.R` in 2026 reproduced the released training tables exactly,
  but gave maps with r 0.95–0.999 against the release, also differing between re-runs. The 387 days on
  which `20200509` and `20200907` are byte-identical are early/late-season days (few snowy stations),
  where the fit does not depend on the folds.
- `20200609_FSCA` / `20200609_LRM_baseline` — 92 / 91 added days (historical day after 2016);
  `20200621_FSCA` — rerun of 2011-01-01 (uses the 2010-12-31 FSCA). Both are merged into `LRM-FSCA_v1` /
  `LRM-baseline_v1` (verified byte-identical: 92/92, 91/91, 1/1; they replace the 0509 file where both exist).
- `20200907_FSCA` — a later full re-run (Sep 2020, through 2019-12-31) that drops stations with nodata
  predictors and sometimes picks a different historical day. **Not** used in the paper or the release.

`pipeline/0_run_LRM.R` is a later (April 2021) near-real-time configuration (`20210401_…`, no historical
SWE) used for the bias-correction work, not for the released data.
