

library(ncdf4)
# library(Thermimage)
library(raster)
library(doParallel)
library(foreach)


# calculate Sierra average SWE for all the products on RC
# Kehan Yang 20181124

ID <- 'PeakSWE_NWM_SWE'
prod_list <- 'NWM'
rc <- T
if(rc == T){
  # the 
  workingDir <- paste0('/scratch/summit/keya6539/P2_LRM_test/Data/final/peak')
}else{
  workingDir <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/peak'
}

tagres <- 500
prefix <- 'UTM500'
Path_basin <- '/projects/keya6539/comparison/Data/template/newsubbasin/subBasins500/'

ftem <- '/scratch/summit/keya6539/P2_LRM_test/Data/template/LRM_SNM_tem/SNM_500.tif'
tem <- raster(ftem)
year <- as.character()


fn <- list.files(workingDir,glob2rx(paste0("*",ID,'*.tif')),full.names = T,recursive = T)
fnname <- list.files(workingDir,glob2rx(paste0("*",ID,'*.tif')),full.names = F,recursive = T)

output_root <-  '/scratch/summit/keya6539/P2_LRM_test/Data/final/peak_STAT'
output_list <- paste0(output_root, '/',ID,'/Image_',prefix)
dir.create(path=output_list,rec=T,showWarnings = T)

output_list_mean <- paste0(output_root,'/',ID,'/Stat_mean_',prefix)
dir.create(path=output_list_mean,rec=T,showWarnings = T)


cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(j = 1:length(fn),.export = c('output_list_mean','output_list','ftem','tem','year'),
        .packages = c('raster','rgdal','ncdf4'))%dopar%{
          
          source('/projects/keya6539/P2_LRM_test/scripts/0functions/reprojection_raster.R')
          
          datestr <-substr(fnname[j],nchar(fnname[j])-7, nchar(fnname[j])-4)
          outname <- paste0(output_list,'/',prod_list,'_',datestr)
          
          if(!file.exists(outname)){
            outname <- reprojection_raster(fn[j], ftem, dir = output_list, prefix = paste0(prod_list,'_',datestr))
            swe <- raster(outname)
            
            # swe <- swe * 1000
            swe[swe < 0] <- NA
            swe[swe > 20000] <- NA
            
            swes <- swe*tem
            means <- cellStats(swes, stat = 'mean', na.rm = T)
            
            basin <- list.files(Path_basin,'.tif',full.names = T)
            basinname <- list.files(Path_basin,'.tif',full.names = F)
            basinname <- sapply(strsplit(basinname,'.tif'),'[',1)
            basin_mean <- NA
            for(k in 1:length(basin)){
              tu_tem <- raster(basin[k])
              swe <- crop(swe, extent(tu_tem))
              swes <- swe*tu_tem
              basin_mean[k] <- cellStats(swes, stat = 'mean', na.rm = T)
              
            } 
            wdata <- cbind.data.frame(datestr, means,t(basin_mean), outname)
            names(wdata) <- c('dates','SM_mean',basinname, 'file')
            outname <- paste0(output_list_mean,'/',prod_list,'_',datestr,'_SM.txt')
            write.csv(wdata, outname, row.names = F)
            
          }
        }

stopCluster(cl)


fn <- list.files(output_list_mean,'.txt',full.names = T)
alldata <- NA
for(i in 1:length(fn)){
  data <- read.csv(fn[i])
  alldata <- rbind.data.frame(alldata, data)
}
alldata <- alldata[-1,]

outwdata <- paste0('/scratch/summit/keya6539/P2_LRM_test/stat/peak_stat/',prod_list,'_',prefix,'_SM_mean_200603.txt')
alldata <- as.data.frame(alldata)
write.csv(alldata, outwdata, row.names = F )
