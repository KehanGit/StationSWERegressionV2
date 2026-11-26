# KH 20180822
# get the peak SWE and Date of different SWE products and stored in two files: PS_***_2004, PD_***_2004, here 2004 means water year
# ***-- Mar, Par, Bld, DIS, DAS, Sch

library(foreach)
library(doParallel)
library(raster)

workingDir <- '/scratch/summit/keya6539/P2_LRM_test/Data/final/SierraAverage/'
ProdID_list <- '20200512_LRM_V1_pl_wfsca_norec'#c('20200509_LRM_V1_pl_wfsca_dm','20200509_LRM_V1_pl_wofsca')
RES_list <- c('500')
folderlist <- paste0(workingDir, ProdID_list,'/Image_UTM',RES_list)


cores <- length(ProdID_list)
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(k = 1:length(ProdID_list),.packages = c('raster'),.export = c('ProdID_list','folderlist'))%dopar%{
  
  Path_out <- '/scratch/summit/keya6539/P2_LRM_test/Data/final/peak/'
  outmaxswelist <- paste0('PeakSWE_',ProdID_list)
  outmaxdatelist <- paste0('PeakDate_', ProdID_list)
  
  fn <- list.files(folderlist[k], '.tif', full.names = T, recursive = T)
  fnname <- list.files(folderlist[k], '.tif', full.names = F, recursive = T)
  
  #from water year 2014 to 2014, 20031001 - 20140930
  for(iyear in 2001:2019){
    startdate <- as.Date(paste0(as.character(iyear-1),'1001'),'%Y%m%d')
    enddate <-  as.Date(paste0(as.character(iyear),'0930'),'%Y%m%d')
    
    subfn <- fn[as.Date(substr(fnname, nchar(fnname)-11, nchar(fnname)-4),'%Y%m%d') >=  startdate]
    subfnname <- fnname[as.Date(substr(fnname, nchar(fnname)-11, nchar(fnname)-4),'%Y%m%d') >=  startdate]
    subfn <- subfn[as.Date(substr(subfnname, nchar(subfnname)-11, nchar(subfnname)-4),'%Y%m%d') <=  enddate]
    datestrlist <- substr(subfnname, nchar(subfnname)-11, nchar(subfnname)-4)
    
    maxswe <- raster(subfn[1])
    maxswe[is.na(maxswe)] <- 0
    
    #default the first day in the folder as the peak SWE date
    maxdate <- maxswe
    datestr <- as.Date(datestrlist[1],'%Y%m%d')
    days <- strftime(datestr,"%j")
    yr <- strftime(datestr,"%Y")
    if(as.numeric(yr) < iyear){
      days <- as.character(as.numeric(days)+92-365) # add 92 days in previous calender year
    }else{
      days <- as.character(as.numeric(days)+92)
    }
    maxdate[] <- as.numeric(days) 
    
    #loop each water year
    for(j in 2:length(subfn)){
      swe <- raster(subfn[j])
      
      datestr <- datestrlist[j]
      datestr <- as.Date(datestr,'%Y%m%d')
      days <- strftime(datestr,"%j")
      yr <- strftime(datestr,"%Y")
      if(as.numeric(yr) < iyear){
        days <- as.character(as.numeric(days)+92-365) # add 92 days in previous calender year
      }else{
        days <- as.character(as.numeric(days)+92)
      }
      swe[swe<0] <- 0
      swe[swe>20000] <- 0
      swe[is.na(swe)] <- 0
      
      
      maxdate[swe[]>maxswe[]] <- days
      maxswe[swe[]>maxswe[]] <- 0
      swe[swe[]<maxswe[]] <- 0
      maxswe <- maxswe + swe
    }
    outfile <- paste0(Path_out, outmaxswelist[k],'_',as.character(iyear),'.tif')
    writeRaster(maxswe,outfile, overwrite=T)
    outfile <- paste0(Path_out, outmaxdatelist[k],'_',as.character(iyear),'.tif')
    writeRaster(maxdate,outfile,overwrite = T)
  }
  
  
}

stopCluster(cl)

