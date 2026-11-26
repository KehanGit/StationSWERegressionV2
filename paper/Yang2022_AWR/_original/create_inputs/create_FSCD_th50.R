# create FSCD but with th 50%; fsca>=50% snow, fsca<50% not snow
# KH 20200418

library(lubridate)
library(raster)
# check fsca time series 
if(F){
  path <- 'D:/Data/SWE/dataset/ori/FSCAWUS'
  fn <- list.files(path, '.tif$', full.names = T, recursive = T)
  startd <- as.Date('20000224','%Y%m%d')
  end <- as.Date('20200310','%Y%m%d')
  end-startd+1
  length(fn)
}

#resample FSCA to LRM SWE extent and also calculate FSCD and FSC --------

if(F){
  path <- 'D:/Data/SWE/dataset/ori/FSCAWUS'
  fn <- list.files(path, '.tif$', full.names = T, recursive = T)
  fnname <- list.files(path, '.tif$', full.names = F, recursive = T)
  path_out <- 'D:/Data/SWE/dataset/med/fsca/FSCA_LRM_WGS'
  if(!dir.exists(path_out)){
    dir.create(path_out)
  }
  
  ftem <- 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask/snm_watermask.tif'
  for(i in 1:length(fn)){
    outname <- paste0(path_out,'/', substr(fnname[i], nchar(fnname[i])-11, nchar(fnname[i])-4),'_fsca.tif')
    if(!file.exists(outname)){
      reprojection_raster(fn[i], ftem, dir = path_out, method = 'bilinear', 
                          prefix = paste0(substr(fnname[i], nchar(fnname[i])-11, nchar(fnname[i])-4),'_fsca'))
    }
  }
  
}




# daily frequency and daily mean ------------------------------------------------------------------
path_fsca <- 'D:/Data/SWE/dataset/med/fsca/FSCA_LRM_WGS'
path_out_fscd <- 'D:/Data/SWE/dataset/med/fsca/LRM_WGS_F_SCD_TH50' # frequency of snow covered date
# path_out_fsca_dm <- 'D:/Data/SWE/dataset/med/fsca/LRM_WGS_FSCA_DM' # daily mean of fsca
if(!dir.exists(path_out_fscd)){
  dir.create(path_out_fscd)
}
# if(!dir.exists(path_out_fsca_dm)){
#   dir.create(path_out_fsca_dm)
# }


fn <- list.files(path_fsca,'.tif', full.names = T)
# startd1 <- as.Date('20110101','%Y%m%d')
for(k in 2000:2019){
  startd <- as.Date(paste0(as.character(k),'1001'),'%Y%m%d')
  endd <- startd+years(1)-days(1)
  #wy 2012
  wy <- substr(fdate2str(endd),1,4)
  subfn <- fn[as.Date(substr(fn, nchar(fn)-16, nchar(fn)-9), "%Y%m%d") %in% seq(startd, endd, by="days")]
  
  for(i in 1:(length(subfn)-1)){
    #get sub fn prior to the date of interest, start from Oct 2
    today <- startd+days(i)
    yesterday <- startd+days(i-1)
    
    yesterdaystr <- fdate2str(yesterday)
    todaystr <- fdate2str(today)
    
    f_today <- subfn[as.Date(substr(subfn, nchar(subfn)-16, nchar(subfn)-9), "%Y%m%d") == today]
    
    f_fscd_yes <- list.files(path_out_fscd, glob2rx(paste0("*", yesterdaystr,'*.tif')), full.names = T)
    
    if(length(f_fscd_yes)==0){
      f_yes <- subfn[as.Date(substr(subfn, nchar(subfn)-16, nchar(subfn)-9), "%Y%m%d") == yesterday]
      r <- raster(f_yes)
      # writeRaster(r, paste0(path_out_fsca_dm,'/dm_fsca_',yesterdaystr,'.tif'))
      r[r<50] <- 0
      r[r >100] <- 0
      r[r>=50] <- 1
      writeRaster(r, paste0(path_out_fscd,'/fscd_',yesterdaystr,'.tif'))
      # f_fsca_dm_yes <- list.files(path_out_fsca_dm, glob2rx(paste0("*", yesterdaystr,'*.tif')), full.names = T)
    }
    f_fscd_yes <- list.files(path_out_fscd, glob2rx(paste0("*", yesterdaystr,'*.tif')), full.names = T)
    
    r_today <- raster(f_today)
    # r_fsca_dm_yes <- raster(f_fsca_dm_yes)
    r_fscd_yes <- raster(f_fscd_yes)
    
    # r_dm_fsca <- (r_fsca_dm_yes * i + r_today)/(i+1)
    r_today[r_today <50] <- 0
    r_today[r_today >100] <- 0
    r_today[r_today >=50] <- 1
    r_fscd <- r_fscd_yes + r_today
    
    # writeRaster(r_dm_fsca, paste0(path_out_fsca_dm,'/dm_fsca_',todaystr,'.tif'))
    writeRaster(r_fscd, paste0(path_out_fscd,'/fscd_',todaystr,'.tif'))
    
  }
  
}
