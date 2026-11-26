# repreject REC=DA for LRM 
# KH 20200418

library(StationSWERegressionV2)
library(lubridate)
library(foreach)
library(doParallel)

#resample all fsca data  ------------------------------------------------

fn <- list.files('D:/Data/SWE/dataset/ori/REC_DA/WGS100_ori','tif', full.name=T, recursive = T)
fnname <- list.files('D:/Data/SWE/dataset/ori/REC_DA/WGS100_ori','tif', full.name=F, recursive = T)
cores <- detectCores()
# cores <- 4
cl <- makeCluster(cores-1)
registerDoParallel(cl)
foreach(i = 1:length(fn),
        .packages = 'StationSWERegressionV2',
        .export = c("fn",'fnname'))%do%{#simulate in reverse will download less data
          outname <- paste0('D:/Data/SWE/dataset/med/REC_DA_WGS500/',substr(fnname[i], nchar(fnname[i])-11, nchar(fnname[i])-4),'_fsca.tif')
          
          if(!file.exists(outname)){
            reprojection_raster(fn[i], ftemp = 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask/snm_watermask.tif',
                                dir= 'D:/Data/SWE/dataset/med/REC_DA_WGS500', method = 'bilinear', 
                                prefix = paste0(substr(fnname[i], nchar(fnname[i])-11, nchar(fnname[i])-4)) )
          }
        }
stopCluster(cl)