# reproject image to the projection of tempolate image

# function 
# Kehan Yang 20190319

#' @param  rimage input  raster
#' @param  rtemp tempolate image
#' @param  dir folder to store temp file 
#' @param method the method used to resample with default 'bilinear'
#' @param prefix 
#' @export
#' 
#' 
#' 
#' 


reprojection_raster <- function(fimage, rtemp, dir, method = 'bilinear', prefix){
  
  temp_extent <- extent(rtemp)
  xmin <- temp_extent[1]
  xmax <- temp_extent[2]
  ymin <- temp_extent[3]
  ymax <- temp_extent[4]
 
  
  outname <- prefix
  outname <- paste0(dir, '/', outname,'.tif')
  
  # gdalUtils is no longer on CRAN; sf::gdal_utils runs the same gdalwarp
  sf::gdal_utils("warp", fimage, outname, options = c(
    "-t_srs", raster::crs(rtemp, asText = TRUE),
    "-tr", raster::res(rtemp)[1], raster::res(rtemp)[2],
    "-r", method,
    "-te", xmin, ymin, xmax, ymax,
    "-overwrite"))

return(outname)
}
