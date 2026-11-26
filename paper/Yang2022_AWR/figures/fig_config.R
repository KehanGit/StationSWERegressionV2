# Paths for the figure scripts (run from paper/Yang2022_AWR/figures/).
# Inputs are read from the author's archive folders; every figure and intermediate
# file is written to FIG_OUT only. Override roots with environment variables.
source("../config.R")

P2_ROOT  <- Sys.getenv("LRM_P2_ROOT",  LRM_INPUT)  # analysis folder: Stat/, data/, figures/<n>/data
P1_ROOT  <- Sys.getenv("LRM_P1_ROOT",  "/Volumes/new_8t/GoogleDrive/Research/research_phd/P1_REC_Compare")
LRM_RUNS <- Sys.getenv("LRM_RUNS",     file.path(LRM_DATA, "med", "LRM"))   # released runs (LRM-FSCA_v1, ...)
EXT_DATA <- Sys.getenv("LRM_EXT_DATA", file.path(LRM_DATA, ".."))            # other SWE products (SNODAS, NWM), shapefiles
SHAPE_DIR <- Sys.getenv("LRM_SHAPE_DIR", "/Volumes/new_8t/Data/watersheds/BasinBoundaryShp")  # basin outlines
FIG_OUT  <- Sys.getenv("LRM_FIG_OUT",  file.path(getwd(), "out"))
dir.create(FIG_OUT, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(tidyr); library(raster) })
