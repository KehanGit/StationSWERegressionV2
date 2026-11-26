# copy the monthly data to a folder and then do a quality contol to pick up the datasets that can be shared
# Kehan Yang 2020-06-20


f_qc <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/data/LRM_FSCA.txt'
data_qc <- read.csv(f_qc, stringsAsFactors = F)
df <- data_qc

# remove the outlines
if(F){
  th_cf = 1
  th_pbias = 500
  th_ml_pn = 40
  
  df <- df[df$cf > th_cf,]
  print(paste('CF:', dim(df)[1]))
  df_ori <- df
  
  OutVals = boxplot(df_ori$r2)$out
  # th_r2 <- max(OutVals)
  df <- df[!df$r2 %in% OutVals,]
  print(paste('R2:', dim(df)[1]))
  
  df$rmse <- df$rmse * 100 # transfer the unit to cm
  OutVals = boxplot(df$rmse)$out
  # th_rmse <- min(OutVals)
  df <- df[!df$rmse %in% OutVals,]
  print(paste('RMSE:', dim(df)[1]))
  
  # OutVals = boxplot(df_ori$pbias)$out
  # th_pbias <- OutVals[5]
  df <- df[df$pbias < th_pbias,]
  print(paste('Pbias:', dim(df)[1]))
 
  # OutVals = boxplot(df$model)$out
  # th_ml_pn <- max(OutVals)
  df <- df[df$model > th_ml_pn,]
  print(paste('Model Pnum:', dim(df)[1]))
}

# get the monthly date 
sub_df <- df[substr(df$Date, 7,8) == '01',]
summary(sub_df)

datelist <- sub_df$Date

# get the list of all LRM-fsca
path_data1 <- 'D:/Data/SWE/dataset/med/LRM/20200509_LRM_V1_pl_wfsca_dm'
path_data2 <- 'D:/Data/SWE/dataset/med/LRM/20200609_FSCA'

fn1 <- list.files(path_data1,'.tif$', full.names = T)
fn2 <- list.files(path_data2,'.tif$', full.names = T)

sub_fn1 <- fn1[substr(fn1, nchar(fn1)-11, nchar(fn1)-4) %in% datelist]
sub_fn2 <- fn2[substr(fn2, nchar(fn2)-11, nchar(fn2)-4) %in% datelist]

copylist <- c(sub_fn1, sub_fn2)
# fill negative pixel by 0
pnegative <- NA
pNA <- NA
vmax <- NA
vmin <- NA
for(i in 1:length(copylist)){
  rswe <- raster(copylist[i])
  datestr <- substr(copylist[i], nchar(copylist[i])-11, nchar(copylist[i])-4)
  vmax <- rbind(vmax, round(max(getValues(rswe), na.rm=T),3))
  vmin <- rbind(vmin, round(min(getValues(rswe), na.rm=T),3))
  pnegative <- rbind(pnegative, sum(getValues(rswe)<0, na.rm=T))
  pNA <- rbind(pNA, sum(!is.na(getValues(rswe))))
}
wdata <- cbind.data.frame(date = substr(copylist, nchar(copylist)-11, nchar(copylist)-4),
                          vmax[-1], vmin[-1], pnegative[-1], pNA[-1])
write.csv(wdata,'D:/Data/SWE/dataset/med/LRM/LRM_SWE_monthly/LRM_Sierra_monthly_stat.txt',row.names = F)
# the results showed no unreasonable high SWE estimates but a few pixels shown negative SWE, and NA

path_out <- 'D:/Data/SWE/dataset/med/LRM/LRM_SWE_monthly'
temp <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
temp2 <- raster('D:/GoogleDrive/Research/P6_2pages_SWEsampling/data/modeltest/input/template/MREC_SNM_WGS_500.tif')
for(i in 1:length(copylist)){
  rswe <- raster(copylist[i])
  datestr <- substr(copylist[i], nchar(copylist[i])-11, nchar(copylist[i])-4)
  
  rswe[rswe<0] <- 0
  print(sum(getValues(rswe)<0, na.rm=T))
  
  rswe[is.na(rswe) & !is.na(temp2)] <- 0
  print(sum(!is.na(getValues(rswe))))
  
  plot(rswe)
  outname <- paste0(path_out, '/LRM_Sierra_monthly_',datestr,'.tif')
  writeRaster(rswe,outname,overwrite=T)
}

# check dates 
dateadd <- c('20110101','20120101','20130501','20150401','20150501','20170501')
fn_add <- fn1[substr(fn1, nchar(fn1)-11, nchar(fn1)-4) %in% dateadd]
data_qc[data_qc$Date %in% dateadd,]

fn_add <- 'D:/Data/SWE/dataset/med/LRM/20200621_FSCA/phvrcn_20110101.tif'
for(i in 1:length(fn_add)){
  rswe <- raster(fn_add[i])
  datestr <- substr(fn_add[i], nchar(fn_add[i])-11, nchar(fn_add[i])-4)
  
  rswe[rswe<0] <- 0
  print(sum(getValues(rswe)<0, na.rm=T))
  
  rswe[is.na(rswe) & !is.na(temp2)] <- 0
  print(sum(!is.na(getValues(rswe))))
  
  plot(rswe)
  outname <- paste0(path_out, '/LRM_Sierra_monthly_',datestr,'.tif')
  writeRaster(rswe,outname,overwrite=T)
}
# fn_add <- fn1[substr(fn1, nchar(fn1)-11, nchar(fn1)-4) %in% datelist]