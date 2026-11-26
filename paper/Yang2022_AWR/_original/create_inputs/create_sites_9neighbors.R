# did not use this file 
# extract values and 8 nbr in the calculation file
library(raster)

path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/med/studyarea'
fn <- list.files(path,glob2rx(paste0('*_500_*.tif$')),full.names = T)

fill_nbr <- function(x, i=3) {
  if(!is.na(x)[i] ) {
    return(x[i])
  } else {
    return(x[i])
  }
}  

for(i in 1:length(fn)){
  r <- raster(fn[i])
  mr <- raster::as.matrix(r)
  rout <- fill_nbr(mr)
  
  
  # r[!is.na(r)] <- 1
  mr <- raster::as.matrix(r)
  rowlist <- which(!is.na(mr[]))
  
  # get neighbers
  r1 <- rowlist+1
  r2 <- rowlist-1
  r3 <- rowlist-nrow(r)
  r4 <- rowlist+nrow(r)
  r5 <- rowlist+nrow(r)+1
  r6 <- rowlist+nrow(r)-1
  r7 <- rowlist-nrow(r)-1
  r8 <- rowlist-nrow(r)+1
  
  # all
  all <- c(rowlist,r1,r2,r3,r4,r5,r6,r7,r8)
  length(all)
  length(rowlist)*9
  
  rout <- mr
  rout[all] <- 1
  wout <- r
  values(wout) <- rout
  writeRaster(wout,paste0(path,'/snowpillow_1500.tif'), overwrite=T)
}

