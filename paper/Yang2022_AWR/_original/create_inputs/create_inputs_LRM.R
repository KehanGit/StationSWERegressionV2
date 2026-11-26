library(RSAGA)#need to have saga installed
library(tidyverse)
library(rgdal)
library(rgeos)
library(raster)

# 1. This file creates most of the input files.
# 2. Download GMTED2010 DEM and use this as the basis for aspect, slope.
# 3. Compute slope and aspect at local scale and regional scale. In this case, 
# the regional scale is ~4km which should account for more generic weather patterns 
# and is the max scale at which WRF simulations suggest are needed to reproduce weather 
# patterns in complex terrain. see rasmussen et al 2011.
# 4. Dissaggregate the 4km slope and aspect back to native dem resolution
# 5. Derive x,y components for all slope and aspect
# 6. Also computed distance to coast and distance to continental divide. 
# These are more complicated and required "burniing" the coastline and divide line into the dem of 
# the entire western US using gdal_rasterize. Then I used r.grow.distance in GRASS to compute the distance 
# to those features. in the case of the distance to divide, pixels west of the divide are negative and pixels 
# east of the divide are positive. see "create mask east/west divide" section below.

#2020-03-22
#kehan yang
#create tempolate based on DEM and water body
data <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/SNM_500_DEM.tif')
watermask <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/snm_watermask2.tif')
wmcut <- crop(watermask, extent(data))
wmcut[wmcut>0] <- 1
writeRaster(wmcut,'D:/GoogleDrive/Research/P2_LRM_test/data/template/snm_watermask3.tif')
plot(wmcut)
data[data < 1500] <- NA
plot(data)
data[!is.na(data)] <- 1




# setup workding dir ----
# setwd('/Users/dosc3612/CUDrive/IDWY')
path <- 'C:/D/SNM_regression/SWE_SNM30'
modeldir='C:/D/SNM_regression/SWE_SNM30'
dir.create(modeldir,rec=TRUE)

# create dem for domain ----
srtm_fns <- dir(path=paste0(path,'/input'),glob2rx('*merge*.tif$'),full.names = T)
srtm_fns #check to make sure you are using the right files
srtm <- raster(srtm_fns[1])
#read data KH
srtm <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/SNM_500_DEM.tif')
plot(srtm)
# gmted2 <- raster(gmted_fns[2])
#the pixels in gmted dataset don't exactly line up with our limits so merge and resample
# res 500m 0.0041666667
# object extent c(-123.8,34.625,-117.099,42.501)
dsrdem <- paste0(path,'/SNM_dem_30m.tif')
objres <- res(srtm)
objextent <- extent(srtm)
crs(srtm)


# polys <- as(grd, "SpatialPolygons")
demext <- extent(srtm)#extent(-123.8,-117.099,34.625,42.501)
polys <- as(demext,'SpatialPolygons')
centroids <- coordinates(polys)
prj <- crs(srtm)
crs(polys) <- prj
plot(polys)
shapefile(polys, 'D:/GoogleDrive/Research/P2_LRM_test/data/template/ROI.shp')
x <- centroids[,1]
y <- centroids[,2]
ex_1.7 <- SpatialPolygonsDataFrame(polys,
                                   data=data.frame(x=x, y=y, row.names=row.names(polys)))
plot(ex_1.7)
writeOGR(ex_1.7,'D:/GoogleDrive/Research/P2_LRM_test/data/template',layer="SNM", driver="ESRI Shapefile")



# lat/long variables ---- 
path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/template'
coords=coordinates(srtm)
lat <- setValues(srtm,coords[,2])
writeRaster(lat,paste0(path,'/SNM_lat.tif'),NAflag=-99,overwrite=TRUE)
lon <- setValues(srtm,coords[,1])
writeRaster(lon,paste0(path,'/SNM_lon.tif'),NAflag=-99,overwrite=TRUE)



# local slope and slope derivatives ----
dem_file = '/SNM_500_DEM.tif'
slope=gdalUtils::gdaldem('slope',paste0(path,dem_file),paste0(path,'/SNM_slope.tif'),'GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)
zness=sin(slope*pi/180)
writeRaster(zness,paste0(path,'/SNM_zness.tif'),NAflag=-99,overwrite=T)
# file.copy('IDWY_zness.tif',file.path(modeldir,'IDWY_zness.tif'))

# local aspect and aspect derivatives ----
aspect=gdalUtils::gdaldem('aspect',paste0(path,dem_file),paste0(path,'/SNM_aspect.tif'),'GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)

northness=cos(aspect*pi/180)
writeRaster(northness,paste0(path,'/SNM_northness.tif'),NAflag=-99,overwrite=T)
# file.copy('IDWY_northness.tif',file.path(modeldir,'IDWY_northness.tif'))

eastness=sin(aspect*pi/180)
writeRaster(eastness,paste0(path,'/SNM_eastness.tif'),NAflag=-99,overwrite=T)
# file.copy('IDWY_eastness.tif',file.path(modeldir,'IDWY_eastness.tif'))

# regional slope and derviatives
dem4km=aggregate(srtm,8,na.rm=F,filename=paste0(path,'/SNM_dem4km.tif'),NAflag=-99,overwrite=T)

slope=gdalUtils::gdaldem('slope',paste0(path,'/SNM_dem4km.tif'),paste0(path,'/SNM_slope4km.tif'),'GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)
zness=sin(slope*pi/180)
writeRaster(zness,paste0(path,'/SNM_zness4km.tif'),NAflag=-99,overwrite=T)
disaggregate(zness,8,filename=paste0(path,'/SNM_regionalzness.tif'),NAflag=-99,overwrite=T)
# file.copy('IDWY_regionalzness.tif',file.path(modeldir,'IDWY_regionalzness.tif'))

# regional aspect and derivatives ----

aspect=gdalUtils::gdaldem('aspect',paste0(path,'/SNM_dem4km.tif'),paste0(path,'/SNM_aspect4km.tif'),'GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)
northness=cos(aspect*pi/180)
writeRaster(northness,paste0(path,'/SNM_northness4km.tif'),NAflag=-99,overwrite=T)
disaggregate(northness,8,filename=paste0(path,'/SNM_regionalnorthness.tif'),NAflag=-99,overwrite=T)
# file.copy('IDWY_regionalnorthness.tif',file.path(modeldir,'IDWY_regionalnorthness.tif'))

eastness=sin(aspect*pi/180)
writeRaster(eastness,paste0(path,'/SNM_eastness4km.tif'),NAflag=-99,overwrite=T)
disaggregate(eastness,8,filename=paste0(path,'/SNM_regionaleastness.tif'),NAflag=-99,overwrite=T)
# file.copy('IDWY_regionaleastness.tif',file.path(modeldir,'IDWY_regionaleastness.tif'))




