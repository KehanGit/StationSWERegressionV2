# daily frequency from last snow free days ------------------------------------------------------------------
library(lubridate)
library(StationSWERegressionV2)
path_fsca <- 'D:/Data/SWE/dataset/med/fsca/FSCA_LRM_WGS'
path_out_cfsca<- 'D:/Data/SWE/dataset/med/fsca/LRM_WGS_CFSCA' # daily mean of fsca


path_fsca <- '/scratch/summit/keya6539/Data/med/FSCA_LRM_WGS'
path_out_cfsca<- '/scratch/summit/keya6539/Data/med/LRM_WGS_CFSCA' # daily mean of fsca
if(!dir.exists(path_out_cfsca)){
  dir.create(path_out_cfsca)
}


fn <- list.files(path_fsca,'.tif', full.names = T)
# startd1 <- as.Date('20110101','%Y%m%d')
for(k in 2001:2019){
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
    f_cfsca_yes <- list.files(path_out_cfsca, glob2rx(paste0("*", yesterdaystr,'*.tif')), full.names = T)
    
    if(length(f_cfsca_yes)==0){
      f_yes <- subfn[as.Date(substr(subfn, nchar(subfn)-16, nchar(subfn)-9), "%Y%m%d") == yesterday]
      r <- raster(f_yes)
      r[r==255] <- NA
      r <- r/100
      writeRaster(r, paste0(path_out_cfsca,'/cfsca_',yesterdaystr,'.tif'))
    }
    f_cfsca_yes <- list.files(path_out_cfsca, glob2rx(paste0("*", yesterdaystr,'*.tif')), full.names = T)
    r_today <- raster(f_today)
    r_today[r_today==255] <-NA
    r_today <- r_today/100
    r_cfsca_yes <- raster(f_cfsca_yes)
    
    r_cfsca_today <- r_today
    
    r_cfsca_today <- r_cfsca_yes + r_today
    r_cfsca_today[r_today==0] <- 0
    
    writeRaster(r_cfsca_today, paste0(path_out_cfsca,'/cfsca_',todaystr,'.tif'), overwrite=T)
    
  }
  
}
