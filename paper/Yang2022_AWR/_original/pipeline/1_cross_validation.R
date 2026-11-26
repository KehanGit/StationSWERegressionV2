# results 1. cross-validation 
# time series ?
# KH 20200507

library(ggplot2)
library(StationSWERegressionV2)
library(scales)
# library(ggpubr)



source('/projects/keya6539/P2_LRM_test/scripts/0functions/LRM_qc.R')


getwy = function(datestr){
  yr <- as.numeric(substr(datestr, 1,4))
  month <- substr(datestr, 5,6)
  yr[as.numeric(month) > 9] = yr[as.numeric(month) >9]+1
  return(yr)
}


source('/projects/keya6539/P2_LRM_test/scripts/0functions/LRM_qc.R')
path_root <- '/scratch/summit/keya6539/P2_LRM_test/output'
path_out <- '/scratch/summit/keya6539/P2_LRM_test/stat/validation/cross_validation/'

th_cf <- 3
th_r2 <- 0.15
th_pbias <- 200
th_rmse <- 25 # the unite is cm
th_ml_pn <- 40
outname <- paste0(path_out,'cv_meg_data_',
                  as.character(th_cf),'_',
                  as.character(th_r2),'_',
                  as.character(th_pbias),'_',
                  as.character(th_rmse),'_',
                  as.character(th_ml_pn),
                  '_3ids.txt')

if(!file.exists(outname)){
  tag <- '20200509_LRM_V1_pl_wfsca_dm'
  df_wofsca <- LRM_qc(path_root,tag, path_out, th_cf, th_r2, th_pbias, th_rmse, th_ml_pn)
  tag <- '20200509_LRM_V1_pl_wofsca'
  df_wfscadm <- LRM_qc(path_root,tag, path_out, th_cf, th_r2, th_pbias, th_rmse, th_ml_pn)
  tag <- '20200512_LRM_V1_pl_wfsca_norec'
  df_worec <- LRM_qc(path_root,tag, path_out, th_cf, th_r2, th_pbias, th_rmse, th_ml_pn)
  meg_data <- merge(df_wofsca, df_wfscadm, by = 'Date')
  meg_data <- merge(meg_data, df_worec, by = 'Date')
  write.csv(meg_data,outname, row.names = F)
}