# Configuration for the Yang et al. (2022, AWR) LRM runs and analysis.
# Every path used by the numbered scripts is set here. Override any of the
# three roots with environment variables instead of editing this file:
#   LRM_INPUT   folder with the run tables (historical days, simulation dates, stations)
#               and input/res500/{phv,watermask}
#   LRM_DATA    folder with the gridded inputs (MODIS FSCA, daily-mean FSCA, SWE reanalysis)
#   LRM_OUTPUT  where model runs are written
# Defaults are the author's archive drives.

LRM_INPUT  <- Sys.getenv("LRM_INPUT",  "/Volumes/new_8t/GoogleDrive/Research/research_phd/P2_LRM_test")
LRM_DATA   <- Sys.getenv("LRM_DATA",   "/Volumes/PHD_8T_1/Data/SWE/dataset")
LRM_OUTPUT <- Sys.getenv("LRM_OUTPUT", file.path(getwd(), "output"))

# --- run tables -------------------------------------------------------------
STATION_FILE <- file.path(LRM_INPUT, "station_20200419.txt")      # daily CDEC pillow SWE (m)
# Historical (analog) SWE day for each simulation date. The main runs used the
# "num_r2" table; the June 2020 gap-fill runs used "best_r2". Checked against the
# released training tables: every released day matches one of the two, and the 79
# days that match only "best_r2" are all gap-fill days.
HISDAY_FILE         <- file.path(LRM_INPUT, "hisdate_SS_num_r2.txt")
HISDAY_FILE_GAPFILL <- file.path(LRM_INPUT, "hisdate_SS_best_r2.txt")
SIMDATE_FILE         <- file.path(LRM_INPUT, "simdate_all_wy_01_19.txt")
SIMDATE_FILE_GAPFILL <- c(`LRM-FSCA`     = file.path(LRM_INPUT, "simdate_all_20200609_FSCA.txt"),
                          `LRM-baseline` = file.path(LRM_INPUT, "simdate_all_20200609_baseline.txt"))

# --- static grids (500 m WGS84, Sierra Nevada) -------------------------------
PATH_PHV  <- file.path(LRM_INPUT, "input", "res500", "phv")        # SNM_<var>.tif
WATERMASK <- file.path(LRM_INPUT, "input", "res500", "watermask", "snm_watermask.tif")

# --- daily grids -------------------------------------------------------------
PATH_FSCA       <- file.path(LRM_DATA, "ori", "FSCAWUS")              # MODIS FSCA, western US (WUS_YYYYMMDD.tif)
PATH_FSCA_WGS   <- file.path(LRM_DATA, "med", "fsca", "FSCA_LRM_WGS") # same, resampled to the model grid
PATH_FSCA_DM    <- file.path(LRM_DATA, "med", "fsca", "LRM_WGS_FSCA_DM") # daily-mean FSCA (dm_fsca_YYYYMMDD.tif)
PATH_REC        <- file.path(LRM_DATA, "med", "REC_DA_WGS500")        # Sierra Nevada snow reanalysis SWE, 500 m (YYYYMMDD.tif, WY1985-2016)

# --- model settings (paper Section 3) ------------------------------------------
PILLOW_NETWORK <- "cdec"
EXTENT <- c(north = 41.375, east = -117.625, south = 35.125, west = -122.375)  # station filter
SNOW_VAR <- "rcn"        # historical SWE pattern from the reanalysis
MIN_SNOW_STATIONS <- 10  # skip days with fewer pillows reporting SWE > 0
CV_SPLITS <- 30          # Monte Carlo cross-validation
CV_TEST_FRACTION <- 0.1
# Random seed. glmnet's CV folds and the Monte Carlo splits are random; each day uses
# set.seed(SEED + YYYYMMDD), so results do not depend on run order or core count.
# The 2020 runs set no seed. Override with the LRM_SEED environment variable.
SEED <- as.integer(Sys.getenv("LRM_SEED", "2022"))

PHV_BASE <- c("lon", "lat", "dem", "eastness", "northness", "regionaleastness", "regionalnorthness",
              "regionalzness", "zness", "wbDistance", "wbHeight", "wd2coast",
              "nwbDistance", "nwbHeight", "nwd2ocean", "swbDistance", "swbHeight", "swd2ocean")
MODELS <- list(
  `LRM-FSCA`     = list(phv = c(PHV_BASE, "vfscadm"), use_fsca_dm = TRUE,  fsca_gt100_to_na = TRUE),
  `LRM-baseline` = list(phv = PHV_BASE,               use_fsca_dm = FALSE, fsca_gt100_to_na = FALSE)
)
