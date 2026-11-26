# extract SWE at stations from Margulis data

#' @export

#' @param PATH_RCNDOWNLOAD_TXT the path to store extracted rec data 
#' @param PATH_Image the path of reconstruced SWE data 
#' @param alltoday.sp the location sp
#' @param alltoday the df obs 
#' @param simdate the target date

#' @return 


get_rec <- function(PATH_RCNDOWNLOAD_TXT, PATH_Image, alltoday.sp, alltoday,simdate){
  # extract swe from images  ------------------------------------------------
  fn_image1 <- list.files(PATH_Image,'01.tif', full.names = T, recursive = T)
  fn_image2 <- list.files(PATH_Image,'15.tif', full.names = T, recursive = T)
  fn_image <- c(fn_image1, fn_image2)
  datelist <- substr(fn_image, nchar(fn_image)-11, nchar(fn_image)-4)
 
  outname <- paste0(PATH_RCNDOWNLOAD_TXT,'/extractedRECSWE_',simdate,'.txt')
  if(!file.exists(outname)){
    print('Extracting reconstructed swe from Margulis data ....')
    rec_data <- NA
    for(i in 1:length(fn_image)){
      rimage <- raster(fn_image[i])
      rimage[is.na(rimage)] <- 0
      datestr <-datelist[i]
      extract_swe <- raster::extract(rimage,alltoday.sp)
      todayswe <- cbind.data.frame(alltoday[,-4], swe=extract_swe,dte=datestr)
      todayswe <- todayswe[complete.cases(todayswe),]
      rec_data <- rbind(rec_data, todayswe)
    }
    write.csv(rec_data[-1,], outname, col.names = F, row.names = F)
  }
 
  return(outname)
}
