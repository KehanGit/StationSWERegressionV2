# StationSWERegressionV2

Daily gridded snow water equivalent (SWE) from a linear regression model (LRM). Each day an
elastic-net regression (glmnet) is trained on snow pillow SWE and applied to every grid cell, with
these predictors:
- physiographic variables
- a historical SWE pattern from a snow reanalysis
- optionally, MODIS fractional snow-covered area (FSCA).

The package extends `stationsweRegression` by Dominik Schneider
([hoargroup/stationsweRegression](https://github.com/hoargroup/stationsweRegression); Schneider &
Molotch, 2016) and was used in:

> Yang, K., Musselman, K. N., Rittger, K., Margulis, S. A., Painter, T. H., & Molotch, N. P. (2022).
> Combining ground-based and remotely sensed snow data in a linear regression model for real-time
> estimation of snow water equivalent. *Advances in Water Resources*, 160, 104075.
> https://doi.org/10.1016/j.advwatres.2021.104075

## Contents

| Path | What |
|---|---|
| `R/`, `man/`, `inst/extdata` | the R package (station inventories, data setup, model fitting, metrics) |
| `paper/Yang2022_AWR/` | code to reproduce the paper: model runs, figures, data packaging. See its README. |
| `main/main.R` | example near-real-time run script (2022 configuration) |
| `vignettes/` | package vignettes from the original package |

## Installation

```r
# install.packages("remotes")
remotes::install_github("KehanGit/StationSWERegressionV2")
```

Requires R ≥ 4.0 and raster, sp, dplyr, tidyr, tibble, purrr, readr, modelr, glmnet, glmnetUtils,
hydroGOF, lubridate. sf is needed for resampling. Tested with R 4.4.

## Data

The paper's daily SWE for WY2001–2019 (LRM-FSCA and LRM-baseline) is on Zenodo, DOI *to be added*.
That record also has the training data, coefficients, cross-validation statistics and figure inputs.

## Citation

Please cite the paper above. Software citation metadata are in `CITATION.cff`.

## License

MIT (see `LICENSE`). Original package © 2016 Dominik Schneider.
