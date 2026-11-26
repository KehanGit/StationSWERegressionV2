


# load functions and libraries ----------------------------------------------------------
library(StationSWERegressionV2)
library(foreach)
library(doParallel)
# source('/home/keya6539/scripts/0restart/StationSWERegressionV2/R/get_stationswe_data.R')
source('/projects/keya6539/P2_LRM_test/scripts/Model_funs/gnet_phvfsca.R')
source('/projects/keya6539/P2_LRM_test/scripts/Model_funs/pickAlpha.R')
# path_root <- 'D:/GoogleDrive/Github/R/Bias_correction/function/'
# source(paste0(path_root,'fdate2str.R'))
# source(paste0(path_root,'reprojection_raster.R'))

# devtools::install_git('https://github.com/KehanGit/StationSWERegressionV2.git')
# local or summit ---------------------------------------------------------
isLocal <- F
if(isLocal){
  PATH_root <- 'D:/GoogleDrive/Research/LRM_test'
}else{
  PATH_root <- '/scratch/summit/keya6539/LRM_test'
}


# Test_type ---------------------------------------------------------------
isHisday <- T
Res500 <- T


# constants ----
RUNNAME = '20200418_LRM_V1_pl_wfsca_dm'
PHVNAME ='res500'
PATH_root <- '/scratch/summit/keya6539/P2_LRM_test'
PATH_OUTPUT=paste0('/scratch/summit/keya6539/P2_LRM_test/output/',RUNNAME)

PHV_VARS = ~lon+lat+dem+eastness+northness+regionaleastness+regionalnorthness+regionalzness+zness+wbDistance+wbHeight+wd2coast+
  nwbDistance+nwbHeight+nwd2ocean+swbDistance+swbHeight+swd2ocean+vfscadm
if(isHisday){
  Hisday <- read.csv(paste0(PATH_root,'/input/hisdate_SS_num_r2.txt'), stringsAsFactors = F)
  # hisday <- read.csv(paste0(PATH_root,'/input/extdata/bestdays.txt'), stringsAsFactors = F)
}
# Data_simulation <- read.csv(paste0(PATH_root,'/input/extdata/bestdays.txt'), stringsAsFactors = F)
Data_simulation <- read.csv(paste0(PATH_root,'/input/simdate_all_wy_01_19.txt'), stringsAsFactors = F)
station_data <- read.csv(paste0(PATH_root,'/input/station_20200419.txt'), stringsAsFactors = F)
# station_data <- station_data[,-1] # remove the first row X

SNOW_VAR = 'rcn'#rcn or fsca
PILLOW_NETWORK = 'cdec' #cdec (for CA) or snotel (for other locations)
EXTENT_NORTH = 41.375
EXTENT_EAST = -117.625
EXTENT_SOUTH = 35.125
EXTENT_WEST = -122.375
# RESO = 15/3600 #resolution in degrees


PATH_input <-paste0(PATH_root,'/input/',PHVNAME,'/')
PATH_PHV=paste0(PATH_input,'phv')
PATH_FSCA=paste0(PATH_input,'fsca')
PATH_FSCA_WGS=paste0(PATH_input,'fsca_resa')
if(!isLocal){
  PATH_FSCA='/scratch/summit/keya6539/Data/SWE/FSCAWUS'
  PATH_FSCA_WGS='/scratch/summit/keya6539/Data/med/FSCA_LRM_WGS'
  # add scd path 
  PATH_FSCD = '/scratch/summit/keya6539/Data/med/LRM_WGS_FSCA_DM'
}
watermask <- raster(paste0(PATH_input,'watermask/snm_watermask.tif'))

# input setup ----
if(isLocal){
  if(Res500){
    PATH_RCNDOWNLOAD='D:/GoogleDrive/Data/SWE/Margulis_data/WGS500_RUN500'#don't need this if predicting with fsca instead of rcn
  }else{
    PATH_RCNDOWNLOAD='D:/GoogleDrive/Data/SWE/Margulis_data/WGS100_ori'#don't need this if predicting with fsca instead of rcn
  }
}else{
  if(Res500){
    PATH_RCNDOWNLOAD='/scratch/summit/keya6539/Data/SWE/REC_DA/REC_DA_WGS500'
    if(!dir.exists(PATH_RCNDOWNLOAD)){
      dir.create(PATH_RCNDOWNLOAD)
    }
    PATH_RCNDOWNLOAD100='/scratch/summit/keya6539/Data/SWE/REC_DA/WGS100_ori'#don't need this if predicting with fsca instead of rcn
  }else{
    PATH_RCNDOWNLOAD='/scratch/summit/keya6539/SWE/Data/SWE/Margulis/WGS100_ori'
    PATH_RCNDOWNLOAD100='/scratch/summit/keya6539/SWE/Data/SWE/Margulis/WGS100_ori'#don't need this if predicting with fsca instead of rcn
    
  }
}

# output directories ----
PATH_XVAL=file.path(PATH_OUTPUT,'crossval_stats_dates')
dir.create(path=PATH_XVAL,rec=TRUE, showWarnings = F)


# get station metadata ----
# filter stations by bounding box using latitude and longitude.
station_locations <- get_inv(PILLOW_NETWORK) %>%
  filter(Latitude>=EXTENT_SOUTH, Latitude<=EXTENT_NORTH, Longitude<= EXTENT_EAST, Longitude>= EXTENT_WEST)

# create a spatial object of the locations ----
snotellocs=as.data.frame(station_locations)
coordinates(snotellocs)= ~Longitude+Latitude
coordnames(snotellocs)=c('x','y')
proj4string(snotellocs)='+proj=longlat +datum=WGS84'
# snotellocs.usgs=spTransform(snotellocs,'+init=epsg:5070')

# get phv variables ----
# get phv variables for entire domain from /data/phv folder and convert to a dataframe
# see use_package and Make_PHV_Inputs vignettes
phvfilenames=dir(PATH_PHV,pattern='.tif$',full.names=TRUE)
phvstack=stack(phvfilenames)
names(phvstack) <- sapply(strsplit(names(phvstack),'_'),'[',2)
if(Res500){
  phvstack_scaled <- raster::scale(phvstack)
}else{
  phvstack_scaled <- phvstack
}

ucophv <- as.data.frame(phvstack_scaled)

# extract the phv variable values for the station locations ----
phvsnotel=raster::extract(phvstack_scaled,snotellocs,sp=T)
phvsnotel=phvsnotel %>%
  tbl_df %>%
  mutate_if(is.factor,as.character) %>%
  dplyr::select(Site_ID,site_name,dem:zness)

cores <- detectCores()
# cores <- 4
cl <- makeCluster(cores)
registerDoParallel(cl)

irow=1
foreach(irow = 1:nrow(Data_simulation),#nrow(whichdates):1,
        # foreach(irow = 1:4,#nrow(whichdates):1,
        .packages = 'StationSWERegressionV2',
        .export = c("RUNNAME", "PATH_OUTPUT","station_locations","PILLOW_NETWORK","PATH_FSCA",
                    "phvsnotel","PHV_VARS","PATH_RCNDOWNLOAD",'PATH_RCNDOWNLOAD100',"watermask","PATH_OUTPUT",
                    "PATH_XVAL",'Hisday','station_data','isLocal','PATH_FSCD','snotellocs'))%dopar%{#simulate in reverse will download less data
                      
                      simdate <- Data_simulation[irow,1]
                      
                      yr=strftime(simdate,'%Y')
                      doy=strftime(simdate,'%j')
                      mth=strftime(simdate,'%m')
                      dy=strftime(simdate,'%d')
                      datestr=paste0(yr,mth,dy)
                      
                      besthisdate <- Hisday$hisdate[Hisday$simdate == simdate]
                      besthisdate <- fdate2str(besthisdate)
                      print(simdate)
                      mapfn=file.path(PATH_OUTPUT,paste0('phvrcn_',datestr,'.tif'))
                      fe.logical=file.exists(mapfn)
                      if(fe.logical) {
                        print(paste0('swe exists in ', PATH_OUTPUT,'. skipping.'))
                        next
                      }
                      
                      target <- paste0("*",datestr,"*.tif")
                      # target <- paste0('*',as.character(yr),"*",doy,'*')
                      fscafile <- list.files(PATH_FSCA, glob2rx(target),full.names = T,recursive = T)
                      if(length(fscafile) == 0){
                        print("Don't have fsca data in this day!")
                        next
                      }else{
                        print(fscafile)
                        simfsca <- raster(fscafile)
                        if(extent(simfsca) != extent(watermask)){
                          # print('fsca need to be resampled')
                          fscafile1 <- list.files(PATH_FSCA_WGS, pattern = glob2rx(target),full.names = T,recursive = T)
                          # simfsca <- raster(fscafile1)
                          if(length(fscafile1) == 0){
                            print('fsca need to be resampled')
                            fscafile2 <- reprojection_raster(fimage = fscafile,watermask,PATH_FSCA_WGS,'bilinear',datestr)
                            simfsca <- raster(fscafile2)
                          }else{
                            simfsca <- raster(fscafile1)
                          }
                          
                        }
                        simfsca[simfsca > 100] <- NA
                        
                        ## subset snotel data for simulation date ----p
                        snoteltoday <-
                          station_data %>%
                          filter(!is.na(Longitude),!is.na(Latitude)) %>%
                          dplyr::filter_(~dte == datestr)#
                        
                        # names(snoteltoday)[5] <- 'pillowswe'
                        
                        truenum <- table(snoteltoday$pillowswe>0)["TRUE"]
                        if( truenum< 10)
                        {
                          print('The number of stations with snow is less than 10! skip this day')
                          print(datestr)
                          next
                        }
                        else
                        {
                          
                          snoteltoday.sp=data.frame(snoteltoday)
                          sp::coordinates(snoteltoday.sp)=~Longitude+Latitude
                          proj4string(snoteltoday.sp)='+proj=longlat +datum=WGS84'
                          
                          #determin if REC SWE exit
                          tag <- paste0('*',besthisdate,'*.tif')
                          fnrec <- list.files(PATH_RCNDOWNLOAD, glob2rx(tag), full.names = T, recursive = T)
                          if(length(fnrec)==1){
                            print('historical REC data exist!')
                            rrec <- raster(fnrec)
                            if(extent(rrec) != extent(watermask)){
                              print('REC needs to be resampled')
                              fnrec <- list.files(PATH_RCNDOWNLOAD100, glob2rx(tag), full.names = T, recursive = T)
                              fnrec2 <- reprojection_raster(fimage = fnrec,watermask,PATH_RCNDOWNLOAD,'bilinear',besthisdate)
                              rrec <- raster(fnrec2)
                            }
                          }else{
                            print('historical REC data is not exist!')
                            fnrec <- list.files(PATH_RCNDOWNLOAD100, glob2rx(tag), full.names = T, recursive = T)
                            fnrec2 <- reprojection_raster(fimage = fnrec,watermask,PATH_RCNDOWNLOAD,'bilinear',besthisdate)
                            print(fnrec2)
                            rrec <- raster(fnrec2)
                          }
                          
                          if(isLocal){
                            source('D:/GoogleDrive/Github/R/StationSWERegressionV2/R/setup_modeldata_oneday.R')
                          }else{
                            source('/projects/keya6539/P2_LRM_test/scripts/Model_funs/setup_modeldata_oneday.R')
                          }
                          
                          # add vfscd data to ucophv and phvsnotel
                          target <- paste0("*",datestr,"*.tif")
                          fn_fscd <- list.files(PATH_FSCD, glob2rx(target), full.names = T, recursive = T)
                          rfscd <- raster(fn_fscd)
                          rfscd_scale <- scale(rfscd)
                          vfscd_scale <- getValues(rfscd_scale)
                          ucophv2 <- cbind.data.frame(ucophv, vfscadm=vfscd_scale)
                          
                          vfscdsnotel=raster::extract(rfscd_scale,snotellocs,sp=F)
                          phvsnotel2 <- 
                            phvsnotel %>%
                            add_column(vfscadm=vfscdsnotel)
                          
                          
                          modelingdFs <- setup_modeldata_oneday(snoteltoday.sp,phvsnotel2,simfsca,
                                                                SNOW_VAR,PHV_VARS,PATH_RCNDOWNLOAD,PATH_OUTPUT,
                                                                besthisdate,
                                                                datestr,phvstack,ucophv2)
                          doidata=modelingdFs[[1]]
                          predictdF=modelingdFs[[2]]
                          myformula=modelingdFs[[3]]
                          
                          ## fit glmnet model ----
                          mdl <- gnet_phvfsca(doidata,myformula)
                          
                          
                          ## predict on swe for domain and mask with fsca and watermask ----
                          yhat=predict(mdl,predictdF,na.action=na.pass)
                          
                          simyhat=simfsca
                          values(simyhat) <- yhat
                          
                          simyhat=simfsca
                          values(simyhat) <- yhat
                          # simyhat <- mask(simyhat,watermask,maskvalue=1,updatevalue=NA)
                          simyhat <- mask(simyhat,simfsca)
                          # Karl's FSCA, gap filled, no flags
                          # simyhat <- mask(simyhat,simfsca,maskvalue>100,updatevalue=250)
                          # simyhat <- mask(simyhat,simfsca,maskvalue=250,updatevalue=250)
                          simyhat <- mask(simyhat,simfsca,maskvalue=0,updatevalue=0)
                          
                          # simyhat[simyhat<0] <- -98
                          # simyhat[simyhat>20] <- NA
                          ## save prediction to file ----
                          outfile=paste0('phv',SNOW_VAR,'_',datestr,'.tif')
                          writeRaster(simyhat,file.path(PATH_OUTPUT,outfile),NAflag=-99,overwrite=T)
                          
                          
                          # maxswe=max(simyhat[simyhat<200])
                          
                          ## save coefficients from model and write to file ----
                          coef_dF <-
                            as.data.frame(as.matrix((coef(mdl)))) %>%
                            mutate(predictor=rownames(.)) %>%
                            setNames(c('coefficient','predictor'))
                          
                          write_tsv(format(coef_dF,sci=FALSE),
                                    path=paste0(PATH_OUTPUT,'/phv',SNOW_VAR,'_coefs_',datestr,'.txt')
                          )
                          
                          ## cross validation statistics  ----
                          print(' - Computing crossvalidation statistics...')
                          fdate2str <- function(simdate){
                            yr=strftime(simdate,'%Y')
                            doy=strftime(simdate,'%j')
                            mth=strftime(simdate,'%m')
                            dy=strftime(simdate,'%d')
                            datestr=paste0(yr,mth,dy)
                            return(datestr)
                          }
                          
                          mypctmae2=function(model,data) {
                            yvar=all.vars(formula(model)[[2]])
                            yhat=predict(model, data)
                            yobs=as.data.frame(data)[[yvar]]
                            df <- cbind.data.frame(yhat,yobs)
                            names(df) <- c('sim','obs')
                            # hydroGOF::pbias(df$sim,df$obs, na.rm=T)
                            hydroGOF::mae(df$sim,df$obs)/mean(yobs,na.rm=T)*100
                            
                            
                          }
                          
                          mypbias=function(model,data) {
                            yvar=all.vars(formula(model)[[2]])
                            yhat=predict(model, data)
                            yobs=as.data.frame(data)[[yvar]]
                            df <- cbind.data.frame(yhat,yobs)
                            names(df) <- c('sim','obs')
                            hydroGOF::pbias(df$sim,df$obs, na.rm=T)
                            
                          }
                          
                          
                          allmdls <-
                            doidata %>%
                            crossv_mc(.,n=30,test=0.1) %>%
                            mutate(
                              dte=datestr,
                              phvfsca_obj_glmmdl=map(train,gnet_phvfsca,myformula),
                              phvfsca_r2_glmmdl=map2_dbl(phvfsca_obj_glmmdl,test,myr2),
                              phvfsca_pctmae_glmmdl=map2_dbl(phvfsca_obj_glmmdl,test,mypctmae2),                            
                              phvfsca_pbias_glmmdl=map2_dbl(phvfsca_obj_glmmdl,test,mypbias)
                              
                            )
                          
                          
                          stat_r2 <-
                            allmdls %>%
                            group_by(dte) %>%
                            summarise(
                              avg_r2=mean(phvfsca_r2_glmmdl,na.rm=T),
                              sd_r2=sd(phvfsca_r2_glmmdl,na.rm=T),
                              uci_r2=avg_r2+1.96*sd_r2/sqrt(n()),
                              lci_r2=avg_r2-1.96*sd_r2/sqrt(n())
                            )
                          
                          write_tsv(stationsweRegression::format_numeric(stat_r2,sci=FALSE),
                                    path=file.path(PATH_XVAL,paste0('phv',SNOW_VAR,'_r2_',datestr,'.txt')))
                          
                          stat_pctmae <-
                            allmdls %>%
                            group_by(dte) %>%
                            summarise(
                              avg_pctmae=mean(phvfsca_pctmae_glmmdl,na.rm=T),
                              sd_pctmae=sd(phvfsca_pctmae_glmmdl,na.rm=T),
                              uci_pctmae=avg_pctmae+1.96*sd_pctmae/sqrt(n()),
                              lci_pctmae=avg_pctmae-1.96*sd_pctmae/sqrt(n())
                            )
                          
                          write_tsv(stationsweRegression::format_numeric(stat_pctmae,sci=FALSE),
                                    path=file.path(PATH_XVAL,paste0('phv',SNOW_VAR,'_pctmae_',datestr,'.txt')))
                          
                          
                          
                          stat_pbias <-
                            allmdls %>%
                            group_by(dte) %>%
                            summarise(
                              avg_pbias=mean(phvfsca_pbias_glmmdl,na.rm=T),
                              sd_pbias=sd(phvfsca_pbias_glmmdl,na.rm=T),
                              uci_pbias=avg_pbias+1.96*sd_pbias/sqrt(n()),
                              lci_pbias=avg_pbias-1.96*sd_pbias/sqrt(n())
                            )
                          
                          write_tsv(stationsweRegression::format_numeric(stat_pbias,sci=FALSE),
                                    path=file.path(PATH_XVAL,paste0('phv',SNOW_VAR,'_pbias_',datestr,'.txt')))
                        }
                      }
                    }
stopCluster(cl)

