
# NWM nc file to raster 
# convert nc file to tif
# get  variable from all the nc file
# store the data with the same name (NWM_YYYYMMDD0000_Variable.tif)
# store the US data 
# store the Sierra study area with the same TEM

library(ncdf4)
library(Thermimage)
library(raster)
library(doParallel)
library(foreach)

# if you use research computer, please set rc=TT
rc <- T
if(rc == T){
  # the 
  workingDir <- '/scratch/summit/keya6539/Data/SWE/NWM_SWE/'
}else{
  workingDir <- 'D:/GoogleDrive/Data/NWM/ArchiveData/2017'
}

setwd(workingDir)

# get all the nc files
fn <- list.files(paste0(workingDir,'/NWMDownload'),'.comp',full.names = T, recursive = T)
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i = 1:length(fn), 
        .export = (c('fn', 'workingDir')),
        .packages = c('ncdf4','Thermimage','raster'))%dopar%{
          
          nc <- nc_open(fn[i])
          
          # get the coor of the data
          x <- ncvar_get(nc,"x")
          y <- ncvar_get(nc,"y")
          nlat <- y
          nlon <- x
          xmin <- x[1]
          xmax <- x[dim(x)]
          ymax <- y[dim(y)]
          ymin <- y[1]
          
          
          # read the variable
          dname <- 'SNEQV'
          swem_array <- ncvar_get(nc,dname)
          fillvalue <- ncatt_get(nc,dname,"_FillValue")
          
          # the info was read from the nc file
          res <- 1000
          mycrs <- '+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs'
          
          #Convert from array to matrix
          swem_arraym <- rotate90.matrix(swem_array)
          
          out <- swem_arraym
          out[out == fillvalue] <- NA
          outr <- raster(out,crs = mycrs)
          
          rasExt  <- extent(xmin,xmax,ymin,ymax)
          extent(outr) <- rasExt
          
          # the unit of the data is mm
          fnname <- list.files(paste0(workingDir,'/NWMDownload'),'.comp',full.names = F, recursive = T)
          
          outpath <- paste0(workingDir,'/CONUS_SWE_0600')
          outdir <- paste0(outpath,'/',substr(fnname[i],1,4))
          if(!dir.exists(outdir)){
            dir.create(outdir, showWarnings = FALSE)
          }
          outname <- paste0(outpath,'/',substr(fnname[i],1,5),'NWM_',substr(fnname[i],6,13),'_SWE.tif')
          writeRaster(outr, outname,overwrite=T)
          
          
        }

stopCluster(cl)

