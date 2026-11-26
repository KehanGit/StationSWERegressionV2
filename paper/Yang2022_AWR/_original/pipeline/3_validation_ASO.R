# function for LRM validation using ASO data 
# inputs include LRM output, tempfile, ASO folder, and date

library(raster)
path_function <- '/projects/keya6539/P2_LRM_test/scripts/0functions/'
source(paste0(path_function,'reprojection_raster.R'))
source(paste0(path_function,'validation_by_ASO_V3.R'))

tagres <- '500'
ftem <- paste0('/projects/keya6539/P2_LRM_test/tem/TU/version3/TU',tagres,'.tif')
tem_tu <- raster(ftem)

# resample to tu -----------------------------------------------------------
runs <- c('LRM-FSCA_v1')
Data_date <- read.csv('/scratch/summit/keya6539/P2_LRM_test/ASO_Date_2019.txt')

for(i in 1:length(runs)){
  Path_list <- paste0('/scratch/summit/keya6539/P2_LRM_test/output/', runs[i])
  Path_resa <- paste0('/scratch/summit/keya6539/P2_LRM_test/Data/final/LRM_ASO_TU/', runs[i])
  Path_asoout <- '/scratch/summit/keya6539/P2_LRM_test/Data/final/LRM_ASO_TU/ASO_500'
  Path_validationOut <- "/scratch/summit/keya6539/P2_LRM_test/stat/validation/ASO/"
  
  if(!dir.exists(Path_resa[i])){
    dir.create(Path_resa[i])
  }
  
  for(j in nrow(Data_date):1){
    tag <- paste0('*',Data_date[j,1],'*.tif')
    print(tag)
    fn <- list.files(Path_list[i],glob2rx(tag),full.names = T,recursive = T)
    fnname <- list.files(Path_list[i],glob2rx(tag),full.names = F, recursive = T)
    if(length(fn)==1){
      if(!file.exists(paste0(Path_resa[i],'/',sapply(strsplit(fnname,'.t'), '[',1),'.tif'))){
        reprojection_raster(fn,ftem, dir=Path_resa[i],'bilinear',sapply(strsplit(fnname,'.t'), '[',1))
      }
    }
  }
  
  # validation by aso -------------------------------------------------------
  lsdate <- NA
  validation_res <- NA
  fn_lrm <- list.files(Path_resa[i],'.tif$',full.names = T)
  fnname_lrm <- list.files(Path_resa[i],'.tif$',full.names = F)
  for(k in 1:length(fn_lrm)){
    rsim <- raster(fn_lrm[k])
    rsim[rsim<0] <- 0
    
    rsim <- rsim * tem_tu
    if( max(getValues(rsim),na.rm=T) < 50){
      rsim <- rsim * 1000
    }
    #remove outliers
    outlier <- boxplot.stats(getValues(rsim))$out
    outlier <- outlier[outlier > max(median(getValues(rsim), na.rm=T), 25000)]
    # print(outlier)
    rsim[rsim %in% outlier] <- NA
    ## find aso data
    lsdate[k] <-  substr(sapply(strsplit(fnname_lrm[k],'.t'),'[',1),nchar(fnname_lrm[k])-11,nchar(fnname_lrm[k])-3)
    
    faso <- list.files(Path_asoout, glob2rx(paste0("*", lsdate[k],'*.tif')), full.names = T)
    if(length(faso)==0){
      next('No aso flight!')
      lsdate[i] <- paste0(lsdate[k], '_no_aso')
    }
    robs <- raster(faso)*1000
    robs <- robs * tem_tu
    
    validation_res <- rbind.data.frame(validation_res, validation_by_ASO_V3(rsim, robs))
  }
  
  
  wdata <- cbind.data.frame(Date = lsdate, validation_res[-1,])
  write.csv(wdata,paste0(Path_validationOut,runs[i],'_ASO_vali.txt'), row.names = F)
  summary(wdata)
  
  
}








