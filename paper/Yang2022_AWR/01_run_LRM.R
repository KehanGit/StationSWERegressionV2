# 01_run_LRM.R — daily LRM SWE runs for Yang et al. (2022, AWR).
#
# Cleaned version of the 2020 run scripts in _original/final_runs_2020/
# (run_LRM-FSCA*.R, run_LRM-baseline*.R). Same steps and settings; paths come from config.R.
#
# Usage (from paper/Yang2022_AWR/):
#   Rscript 01_run_LRM.R <MODEL> [main|gapfill] [--dates 2017-04-01,2017-04-02] [--cores N]
#     MODEL    LRM-FSCA or LRM-baseline
#     main     simulation dates in SIMDATE_FILE with HISDAY_FILE (default)
#     gapfill  the June 2020 gap-fill dates with HISDAY_FILE_GAPFILL
# Output: LRM_OUTPUT/<MODEL>_<set>/ with, per day,
#   phvrcn_YYYYMMDD.tif (SWE, m, nodata -99), modeldata_YYYYMMDD.txt, phvrcn_coefs_YYYYMMDD.txt,
#   crossval_stats_dates/phvrcn_{r2,rmse,pctmae,pbias}_YYYYMMDD.txt
# The released dataset = main run + gap-fill days (+ the 2011-01-01 rerun, see README).
#
# Runs are repeatable (fixed seed per day, SEED in config.R). The 2020 runs set no seed, so re-runs
# are close to but not bit-identical with the released files.

suppressPackageStartupMessages({
  library(raster); library(sp); library(dplyr); library(tidyr); library(tibble)
  library(purrr); library(readr); library(modelr); library(glmnet); library(glmnetUtils)
})
source("config.R")
for (f in list.files("functions", "\\.R$", full.names = TRUE)) source(f)
REPO_ROOT <- normalizePath(file.path("..", ".."))

# ---- arguments ------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
MODEL <- args[1]
if (is.na(MODEL) || !MODEL %in% names(MODELS)) stop("first argument must be one of: ", paste(names(MODELS), collapse = ", "))
RUNSET <- if (length(args) >= 2 && args[2] %in% c("main", "gapfill")) args[2] else "main"
opt <- function(flag) { i <- match(flag, args); if (is.na(i)) NA else args[i + 1] }
cores <- if (is.na(opt("--cores"))) max(1, parallel::detectCores() - 1) else as.integer(opt("--cores"))

m <- MODELS[[MODEL]]
PHV_VARS <- as.formula(paste("~", paste(m$phv, collapse = " + ")))
Hisday <- read.csv(if (RUNSET == "main") HISDAY_FILE else HISDAY_FILE_GAPFILL, stringsAsFactors = FALSE)
simdates <- read.csv(if (RUNSET == "main") SIMDATE_FILE else SIMDATE_FILE_GAPFILL[[MODEL]], stringsAsFactors = FALSE)[[1]]
if (!is.na(opt("--dates"))) simdates <- strsplit(opt("--dates"), ",")[[1]]
station_data <- read.csv(STATION_FILE, stringsAsFactors = FALSE)

PATH_OUTPUT <- file.path(LRM_OUTPUT, paste0(MODEL, "_", RUNSET))
PATH_XVAL <- file.path(PATH_OUTPUT, "crossval_stats_dates")
PATH_RESAMPLED <- file.path(LRM_OUTPUT, "fsca_resampled")
dir.create(PATH_XVAL, recursive = TRUE, showWarnings = FALSE)
watermask <- raster(WATERMASK)

# ---- stations and physiographic predictors -------------------------------------------
station_locations <- get_cdec_inventory(REPO_ROOT) %>%
  filter(Latitude >= EXTENT[["south"]], Latitude <= EXTENT[["north"]],
         Longitude <= EXTENT[["east"]], Longitude >= EXTENT[["west"]])
snotellocs <- as.data.frame(station_locations)
coordinates(snotellocs) <- ~Longitude + Latitude
coordnames(snotellocs) <- c("x", "y")
proj4string(snotellocs) <- "+proj=longlat +datum=WGS84"

phvstack <- stack(dir(PATH_PHV, pattern = ".tif$", full.names = TRUE))
names(phvstack) <- sapply(strsplit(names(phvstack), "_"), "[", 2)
phvstack_scaled <- raster::scale(phvstack)          # predictors standardised over the domain
ucophv <- as.data.frame(phvstack_scaled)
phvsnotel <- raster::extract(phvstack_scaled, snotellocs, sp = TRUE) %>%
  as_tibble() %>% mutate_if(is.factor, as.character) %>%
  dplyr::select(Site_ID, site_name, dem:zness)

find1 <- function(dir, datestr) list.files(dir, glob2rx(paste0("*", datestr, "*.tif")), full.names = TRUE, recursive = TRUE)

# ---- one model day -----------------------------------------------------------------------
run_day <- function(simdate) {
  datestr <- fdate2str(simdate)
  set.seed(SEED + as.integer(datestr))
  if (file.exists(file.path(PATH_OUTPUT, paste0("phvrcn_", datestr, ".tif")))) return("exists")
  besthisdate <- fdate2str(Hisday$hisdate[Hisday$simdate == simdate])

  # MODIS FSCA on the model grid
  f <- find1(PATH_FSCA_WGS, datestr)
  if (length(f) == 0) {
    src <- find1(PATH_FSCA, datestr)
    if (length(src) == 0) return("no FSCA")
    dir.create(PATH_RESAMPLED, showWarnings = FALSE)
    f <- resample_to_grid(src[1], watermask, file.path(PATH_RESAMPLED, paste0(datestr, ".tif")))
  }
  simfsca <- raster(f[1])
  if (m$fsca_gt100_to_na) simfsca[simfsca > 100] <- NA

  snoteltoday <- station_data %>% filter(!is.na(Longitude), !is.na(Latitude), dte == datestr)
  if (sum(snoteltoday$pillowswe > 0, na.rm = TRUE) < MIN_SNOW_STATIONS) return("too few stations with snow")
  snoteltoday.sp <- data.frame(snoteltoday)
  coordinates(snoteltoday.sp) <- ~Longitude + Latitude
  proj4string(snoteltoday.sp) <- "+proj=longlat +datum=WGS84"

  ucophv2 <- ucophv; phvsnotel2 <- phvsnotel
  if (m$use_fsca_dm) {                               # daily-mean FSCA as an extra predictor
    rfscd_scale <- scale(raster(find1(PATH_FSCA_DM, datestr)))
    ucophv2 <- cbind.data.frame(ucophv, vfscadm = getValues(rfscd_scale))
    phvsnotel2 <- add_column(phvsnotel, vfscadm = raster::extract(rfscd_scale, snotellocs, sp = FALSE))
  }

  md <- setup_modeldata_oneday(snoteltoday.sp, phvsnotel2, simfsca, SNOW_VAR, PHV_VARS, PATH_REC,
                               PATH_OUTPUT, besthisdate, datestr, phvstack, ucophv2)
  doidata <- md[[1]]; predictdF <- md[[2]]; myformula <- md[[3]]

  # fit, predict, mask with FSCA (no FSCA -> NA, FSCA = 0 -> SWE 0)
  mdl <- gnet_phvfsca(doidata, myformula)
  simyhat <- simfsca
  values(simyhat) <- predict(mdl, predictdF, na.action = na.pass)
  simyhat <- mask(simyhat, simfsca)
  simyhat <- mask(simyhat, simfsca, maskvalue = 0, updatevalue = 0)
  writeRaster(simyhat, file.path(PATH_OUTPUT, paste0("phv", SNOW_VAR, "_", datestr, ".tif")), NAflag = -99, overwrite = TRUE)

  coef_dF <- as.data.frame(as.matrix(coef(mdl))) %>% mutate(predictor = rownames(.)) %>% setNames(c("coefficient", "predictor"))
  write_tsv(format(coef_dF, sci = FALSE), file.path(PATH_OUTPUT, paste0("phv", SNOW_VAR, "_coefs_", datestr, ".txt")))

  # Monte Carlo cross-validation: 30 random 90/10 splits of the day's stations
  allmdls <- doidata %>% crossv_mc(n = CV_SPLITS, test = CV_TEST_FRACTION) %>%
    mutate(dte = datestr, mdl = map(train, gnet_phvfsca, myformula),
           r2 = map2_dbl(mdl, test, myr2), pctmae = map2_dbl(mdl, test, mypctmae2),
           pbias = map2_dbl(mdl, test, mypbias), rmse = map2_dbl(mdl, test, myrmse))
  for (s in c("r2", "rmse", "pctmae", "pbias")) {
    v <- allmdls[[s]]
    st <- tibble(dte = datestr, avg = mean(v, na.rm = TRUE), sd = sd(v, na.rm = TRUE))
    st <- mutate(st, uci = avg + 1.96 * sd / sqrt(length(v)), lci = avg - 1.96 * sd / sqrt(length(v)))
    names(st)[-1] <- paste0(c("avg_", "sd_", "uci_", "lci_"), s)
    write_tsv(format_numeric(st, sci = FALSE), file.path(PATH_XVAL, paste0("phv", SNOW_VAR, "_", s, "_", datestr, ".txt")))
  }
  "done"
}

# ---- run -----------------------------------------------------------------------------------
RNGkind("L'Ecuyer-CMRG")  # stream-safe RNG for forked workers
message(MODEL, " / ", RUNSET, ": ", length(simdates), " dates, ", cores, " cores, seed ", SEED, " -> ", PATH_OUTPUT)
res <- parallel::mclapply(simdates, mc.set.seed = TRUE, FUN = function(d) tryCatch(run_day(d), error = function(e) paste("error:", conditionMessage(e))),
                          mc.cores = cores)
for (i in seq_along(res)) message(simdates[i], ": ", substr(res[[i]], 1, 400))
