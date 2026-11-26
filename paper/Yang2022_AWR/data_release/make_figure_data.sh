#!/bin/sh
# Copy the inputs of figures/fig*.R into <OUT>/figure_data, keeping the folder layout the scripts
# expect. Run the figures with:  LRM_P2_ROOT=<OUT>/figure_data LRM_SHAPE_DIR=<OUT>/figure_data/shapes
# Usage: make_figure_data.sh P2_ROOT SHAPE_DIR OUT
set -eu
P2=$1; SHP=$2; OUT=$3/figure_data
cd "$P2"
for f in \
  Stat/snow_pillows_used_20210131.txt \
  figures/1_cross_validation/data/v2/LRM_FSCA.txt figures/1_cross_validation/data/v2/LRM_baseline.txt \
  figures/2_validation_snow_course/LRM_date_mdGT40.txt \
  data/studyarea/snowcourse_info.txt data/studyarea/snowcourse_info_2kmbuff.txt \
  data/template/LRM_SNM_tem/SNM_500.tif data/template/LRM_SNM_tem/SNM_500_WM.tif \
  figures/6_peakSWE/SNM_DEM.tif \
  Stat/streamflow/sierramean/20200509_LRM_V1_pl_wfsca_dm_UTM500_SM_mean_200428.txt \
  Stat/streamflow/sierramean/20200509_LRM_V1_pl_wofsca_UTM500_SM_mean_200428.txt \
  Stat/streamflow/sierramean/SNODAS_UTM500_SM_mean_200428.txt \
  Stat/streamflow/sierramean/NWM_SWE_UTM500_SM_mean_200428.txt \
  data/final/pixel_wise_peak_mean/plot/1PeakSWE_LRM_FSCA.tif data/final/pixel_wise_peak_mean/plot/2PeakSWE_LRM_baseline.tif \
  data/final/pixel_wise_peak_mean/plot/4PeakSWE_SNODAS.tif data/final/pixel_wise_peak_mean/plot/5PeakSWE_NWM_SWE.tif \
  figures/2_validation_snow_course/data/*_sc_review.txt \
  figures/3_validation_ASO/data/*_ASO_vali.txt
do mkdir -p "$OUT/$(dirname "$f")"; cp -p "$f" "$OUT/$f"; done
for p in wfscadm wofsca SNODAS NWM norec; do for d in 20130403 20140407 20150217 20160326 20170401; do
  f=figures/3_validation_ASO/data/images/ASO_Diff/$p/${p}_$d.tif; mkdir -p "$OUT/$(dirname "$f")"; cp -p "$f" "$OUT/$f"
done; done
mkdir -p "$OUT/shapes"; cp -p "$SHP"/TuolumneBasin_ASO_UTM11.* "$OUT/shapes/"
find "$OUT" -name '._*' -delete
echo "figure_data: $(find "$OUT" -type f | wc -l) files, $(du -sh "$OUT" | cut -f1)"
