# Small helpers used by the paper runs. Copied from the package R/ folder (or from the
# 2020 run scripts) so the paper pipeline does not need the full package installed.
# Logic unchanged except where noted.

fdate2str <- function(simdate) format(as.Date(simdate), "%Y%m%d")

format_numeric <- function(x, ...) {
  numeric_cols <- vapply(x, is.numeric, logical(1))
  x[numeric_cols] <- lapply(x[numeric_cols], format, ...)
  x
}

# CDEC snow pillow inventory shipped with the package (unchanged since 2019-04).
# Was StationSWERegressionV2::get_inv('cdec') in 2020, now get_station_inventory('cdec').
get_cdec_inventory <- function(repo_root) {
  readr::read_csv(file.path(repo_root, "inst", "extdata", "cdec_swe_inventory.csv"),
                  show_col_types = FALSE)
}

# Resample a raster file onto the model grid (bilinear), as reprojection_raster() did with
# gdalUtils::gdalwarp. Uses sf's bundled GDAL because gdalUtils is no longer on CRAN.
resample_to_grid <- function(src, template, dst) {
  e <- raster::extent(template)
  sf::gdal_utils("warp", src, dst, options = c(
    "-t_srs", raster::crs(template, asText = TRUE),
    "-tr", raster::res(template)[1], raster::res(template)[2],
    "-r", "bilinear", "-te", e@xmin, e@ymin, e@xmax, e@ymax, "-overwrite"))
  dst
}

# ---- cross-validation metrics (from the 2020 run scripts / package error_functions.R) ----
.obs_sim <- function(model, data) {
  yvar <- all.vars(formula(model)[[2]])
  list(sim = as.numeric(predict(model, data)), obs = as.data.frame(data)[[yvar]])
}
myr2      <- function(model, data) { d <- .obs_sim(model, data); stats::cor(d$sim, d$obs, use = "complete.obs")^2 }
mypctmae2 <- function(model, data) { d <- .obs_sim(model, data); hydroGOF::mae(d$sim, d$obs) / mean(d$obs, na.rm = TRUE) * 100 }
mypbias   <- function(model, data) { d <- .obs_sim(model, data); hydroGOF::pbias(d$sim, d$obs, na.rm = TRUE) }
myrmse    <- function(model, data) { d <- .obs_sim(model, data); hydroGOF::rmse(d$sim, d$obs, na.rm = TRUE) }
