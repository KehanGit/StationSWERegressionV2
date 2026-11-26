# Main function to run LRM
# Kehan Yang (github.com/KehanGit), 2022-08-01



# load functions and libraries ----------------------------------------------------------
# library(stationsweRegression) # old package developed by Dominik Schneider which may not working - 20220801
# if this is the first time you run the code, please install the package first
setwd("~/Documents/program/StationSWERegressionV2/main")
install.packages("../StationSWERegressionV2-version0.1.tar", repos = NULL)
library(StationSWERegressionV2)
# do not show warnings
options(warn=-1)
# show warnings
# options(warn=-0)

library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE) # get rid of all warnings



# set up model running environment -----
PATH_root <- '../data'
# change to 'T' if you know the best historic reconsturced SWE; Not for the first run
ishisday <- F
isCCR <- T # change to 'F' if you don't want to use cocorahs data
oldestDate <-as.Date('20000601','%Y%m%d')

# the list of the days need to run the model
simulationday <- read.csv(paste0(PATH_root,'/inputs/simulation_date.txt'), stringsAsFactors = F, header = T)
print(simulationday)


SNOW_VAR = 'rcn'#rcn or fsca
RUNNAME = '20220731_Allhis_noccr_0120' # change the name of output folder




RESO = 15/3600 #resolution in degrees
SIMNAME = 'wRCN_CanAdj_MODSCAG' #change this to separate different simulations
PATH_MODSCAGDOWNLOAD='/Volumes/hydroData/WestUS_Data/MODSCAG/modscag-historic'#modscag'#always need this. path should point 1 level above /yr/doy/*.tif
MODSCAG_FILE = 'snow_fraction_canadj'
FVEG_CORRECTION <- F
MODSCAG_TYPE = 'historic'#'NRT'# 'NRT' or 'historic'

# file contains historical reconstructed SWE at snow pillow locations
PATH_RCNDOWNLOAD_TXT=paste0(PATH_root,'/inputs/RECTXT')
# directory to store downloaded CCR data
PATH_CCR <- paste0(PATH_root,'/inputs/CCR')
dir.create(PATH_CCR)
PATH_SNOTEL <- paste0(PATH_root,'/inputs/snotel')
# PATH_FSCA='/Volumes/hydroData/WestUS_Data/Regress_SWE/SNM/Leanne/snm/data/fsca'
PATH_FSCA =paste0(PATH_root,'/inputs/FSCA/')
PATH_DMFSCA =paste0(PATH_root,'/inputs/DMFSCA')
PATH_RCNDOWNLOAD = paste0(PATH_root,'/inputs/REC_DA/')
# PATH_RCNDOWNLOAD='/Volumes/Users/Kehan/LRM_update/LRM_20220729/input/REC_DA_WGS500'

# temporary directory to save reprojected reconstructed SWE and FSCA
# if the original data are not in the same projection
PATH_temp <- paste0(PATH_root,'/inputs/temp')
PATH_FSCA_WGS <- paste0(PATH_temp,'/fsca_resa') # resampled files
PATH_REC_WGS <- paste0(PATH_temp,'/rec_resa')

dir.create(PATH_temp)
dir.create(PATH_FSCA_WGS)
dir.create(PATH_REC_WGS)

#read input data
PATH_input <-paste0(PATH_root,'/inputs/')
PATH_PHV=paste0(PATH_input,'phv')
fwatermask <- paste0(PATH_input,'/snm_watermask.tif')
watermask <- raster(fwatermask)

# output directories ----
PATH_OUTPUT=paste0(PATH_root,'/outputs/',RUNNAME)
PATH_XVAL=file.path(PATH_OUTPUT,'crossval_stats_dates')
dir.create(path=PATH_XVAL,rec=TRUE, showWarnings = F)

#with barrier variables
PHV_VARS = ~lon+lat+dem+eastness+northness+regionaleastness+regionalnorthness+regionalzness+zness+wbDistance+wbHeight+wd2coast+nwbDistance+
  nwbHeight+nwd2ocean+swbDistance+swbHeight+swd2ocean+dmfsca
# get best historic date for reconstruction swe


# get phv variables ----
# get phv variables for entire domain from /data/phv folder .and convert to a dataframe
# see use_package and Make_PHV_Inputs vignettes
phvfilenames=dir(PATH_PHV,pattern='.tif$',full.names=TRUE)
phvstack=stack(phvfilenames)
names(phvstack) <- sapply(strsplit(names(phvstack),'_'),'[',2)
phvstack_scaled <- scale(phvstack)
ucophv <- as.data.frame(phvstack_scaled)

# get station locations
PILLOW_NETWORK = 'cdec' #cdec (for CA) or snotel (for other locations)
# EXTENT_NORTH = 41.375
# EXTENT_EAST = -117.625
# EXTENT_SOUTH = 35.125
# EXTENT_WEST = -122.375

# get extent based on phv inputs - 20200117 KH
EXTENT_NORTH = extent(phvstack[[1]])[4]
EXTENT_EAST = extent(phvstack[[1]])[2]
EXTENT_SOUTH = extent(phvstack[[1]])[3]
EXTENT_WEST = extent(phvstack[[1]])[1]


# get station metadata ----
# filter stations by bounding box using latitude and longitude.
station_locations <- get_station_inventory(PILLOW_NETWORK) %>%
  filter(Latitude>=EXTENT_SOUTH, Latitude<=EXTENT_NORTH, Longitude<= EXTENT_EAST, Longitude>= EXTENT_WEST)

# create a spatial object of the locations ----
snotellocs=as.data.frame(station_locations)
coordinates(snotellocs)= ~Longitude+Latitude
coordnames(snotellocs)=c('x','y')
proj4string(snotellocs)='+proj=longlat +datum=WGS84'



# extract the phv variable values for the station locations ----
phvsnotel=raster::extract(phvstack_scaled,snotellocs,sp=T)
phvsnotel=phvsnotel %>%
  tbl_df %>%
  mutate_if(is.factor,as.character) %>%
  dplyr::select(Site_ID,site_name,dem:zness)
# only run the model with CCR available
# irow = irow -1
irow=nrow(simulationday) # the model will run throught the dates from the botton of the file to the first date
simulationday$datestr = sapply(simulationday[,1], fdate2str)
# irow = 1 # the model will only run for the first day on the file
oldestDate <- as.Date(as.character(max(simulationday$datestr)), '%Y%m%d') # this is used to determine weather or not to download the station SWE
# if data <= oldestDate, then the model will not download station SWE, otherwise, it will download the newest station data.

# for(irow in nrow(simulationday):1){#simulate in reverse will download less data
for(irow in irow:1){#simulate in reverse will download less data

  simdate <- simulationday[irow,1]
  simdate <- as.Date(as.character(simdate), '%Y-%m-%d')
  yr=strftime(simdate,'%Y')
  doy=strftime(simdate,'%j')
  mth=strftime(simdate,'%m')
  dy=strftime(simdate,'%d')
  datestr=paste0(yr,mth,dy)

  print(simdate)
  mapfn=file.path(PATH_OUTPUT,paste0('phvrcn_',datestr,'.tif'))
  fe.logical=file.exists(mapfn)
  if(fe.logical) {
    print(paste0('swe exists in ', PATH_OUTPUT,'. skipping.'))
    next
  }
  ## download station swe data for the year of simulation date and merge with station locations ----
  print('Get station data----')
  station_data=get_stationswe_data(PATH_SNOTEL,station_locations,PILLOW_NETWORK, simdate, oldestDate)

  # station_data$dte = as.Date(as.character(station_data$dte), '%Y%m%d')
  if(ishisday){
    # you can set the historic data using in model here to save time
    # notice that only the '01' and '15' date could be used considering the computing efficiency
    # for example
    besthisdate <- '20140415'
  }else{
    besthisdate = get_best_historical_date(datestr, station_data)
  }
  print(paste0('The histroical reconstruction SWE on ', besthisdate, ' will be used!'))

  # the function may not working if you do not have access to snowserver
  # simfsca <- get_modscag_data(doy,yr,MODSCAG_TYPE,PATH_FSCA,MODSCAG_FILE,FVEG_CORRECTION,RESO,EXTENT_WEST,EXTENT_EAST,EXTENT_SOUTH,EXTENT_NORTH)
  #or if you have a fsca mosaic created already, read it directly;
  fsimfsca <- list.files(PATH_FSCA, glob2rx(paste0('*',datestr,'*.tif')), full.names = T)
  simfsca <- raster(fsimfsca)
  #if input fsca has different extent with watermask, we need to resample the fsca
  if(!compareRaster(watermask,simfsca, extent=T,rowcol=T, crs=T, res=T, rotation=T,stopiffalse = F)){
    fsimfsca = reprojection_raster(fimage = fsimfsca, rtemp = watermask, dir = PATH_FSCA_WGS, method = 'near',prefix = datestr)

  }
  simfsca <- raster(fsimfsca)
  # if the range of fsca is from 0 - 100.
  simfsca[simfsca > 100] <- NA


  if(isCCR){
    cos_density = 0.1 # the default density for freshsnow
    ccrdata <- get_ccr(datestr, PATH_CCR,cos_density, phvstack[[1]]) # cos_density = 0.1
    print('The transfored cocorahs data:')
    print(ccrdata)
  }



  ## subset snotel data for simulation date ----p
  snoteltoday <-
    station_data %>%
    filter(!is.na(Longitude),!is.na(Latitude)) %>%
    dplyr::filter_(~dte == datestr)#


  if(isCCR){
    ccr <- cbind.data.frame(Site_ID = ccrdata$Site_ID, Longitude = ccrdata$Longitude,
                            Latitude = ccrdata$Latitude, dte = fdate2str(ccrdata$dte), pillowswe = ccrdata$swe)

    alltoday <- rbind(snoteltoday, ccr)
    print(paste0(nrow(ccr),' CCR obs used in the model with mean SWE of ',
                 round(mean(ccr$pillowswe),2),
                 ' and ', sum(ccr$pillowswe>0), ' positive obs'))

    ccrlocs=as.data.frame(ccr)
    coordinates(ccrlocs)= ~Longitude+Latitude
    coordnames(ccrlocs)=c('x','y')
    proj4string(ccrlocs)='+proj=longlat +datum=WGS84'

    # extract the phv variable values for the station locations ----
    phvccr=raster::extract(phvstack_scaled,ccrlocs,sp=T)
    phvccr=phvccr %>%
      tbl_df %>%
      mutate_if(is.factor,as.character) %>%
      dplyr::select(Site_ID, dem:zness)
    phvccr$site_name = phvccr$Site_ID

    phvall<- rbind(phvsnotel, phvccr)

  }else{
    alltoday <- snoteltoday
    phvall <- phvsnotel
  }


  # alltoday <- alltoday[complete.cases(alltoday),]
  alltoday.sp=data.frame(alltoday)
  sp::coordinates(alltoday.sp)=~Longitude+Latitude
  proj4string(alltoday.sp)='+proj=longlat +datum=WGS84'

  truenum <- table(alltoday$pillowswe>0)["TRUE"]
  if( truenum< 10){
    print('The number of stations with snow is less than 10! skip this day')
    print(datestr)
    next
  }



  # add fscddm data to ucophv and phvsnotel
  target <- paste0('*',datestr,"*.tif$")
  fn_dmfsca <- list.files(PATH_DMFSCA, glob2rx(target), full.names = T, recursive = T)
  r_demfsca <- raster(fn_dmfsca)
  r_demfsca[r_demfsca>100] <- NA
  r_demfsca_scale <- scale(r_demfsca)
  v_demfsca_scale <- getValues(r_demfsca_scale)
  ucophv2 <- cbind.data.frame(ucophv, dmfsca=v_demfsca_scale)

  vfscdsnotel=raster::extract(scale(simfsca),alltoday.sp,sp=F)
  df_dmfsca = data.frame(Site_ID = alltoday.sp$Site_ID, dmfsca  =vfscdsnotel)


  phvall2 <- merge(phvall,df_dmfsca, by = 'Site_ID')

  ## run model ---


  modelingdFs <- setup_modeldata_oneday(alltoday.sp,phvall2,simfsca,
                                        SNOW_VAR,PHV_VARS,PATH_OUTPUT,PATH_RCNDOWNLOAD,
                                        fdate2str(besthisdate),
                                        datestr,phvstack,ucophv2, 'glmnet',PATH_REC_WGS)




  doidata=modelingdFs[[1]]
  predictdF=modelingdFs[[2]]
  myformula=modelingdFs[[3]]

  ## fit glmnet model ----
  mdl <- gnet_phvfsca(doidata,myformula,model = 'glmnet')

  ## predict on swe for domain and mask with fsca and watermask ----
  yhat=predict(mdl,predictdF,na.action=na.pass)

  simyhat=simfsca
  values(simyhat) <- yhat


  simyhat=simfsca
  values(simyhat) <- yhat
  simyhat <- mask(simyhat,watermask,maskvalue=1,updatevalue=NA)
  simyhat <- mask(simyhat,simfsca)
  simyhat <- mask(simyhat,simfsca,maskvalue=235,updatevalue=NA)
  simyhat <- mask(simyhat,simfsca,maskvalue=250,updatevalue=NA)
  simyhat <- mask(simyhat,simfsca,maskvalue=0,updatevalue=0)

  simyhat[simyhat<0] <- 0
  # simyhat[simyhat>20] <- NA
  plot(simyhat)
  ## save prediction to file ----
  outfile=paste0('phv',SNOW_VAR,'_',datestr,'.tif')

  writeRaster(simyhat,file.path(PATH_OUTPUT,outfile),NAflag=-99,overwrite=T)

  ## save coefficients from model and write to file ----
  coef_dF <-
    as.data.frame(as.matrix((coef(mdl)))) %>%
    mutate(predictor=rownames(.)) %>%
    setNames(c('coefficient','predictor'))

  write_tsv(format(coef_dF,sci=FALSE),
            path=paste0(PATH_OUTPUT,'/phv',SNOW_VAR,'_coefs_',datestr,'.txt')
  )

  print(' - Computing crossvalidation statistics...')
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

  write_tsv(StationSWERegressionV2::format_numeric(stat_r2,sci=FALSE),
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

  write_tsv(StationSWERegressionV2::format_numeric(stat_pctmae,sci=FALSE),
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

  write_tsv(StationSWERegressionV2::format_numeric(stat_pbias,sci=FALSE),
            path=file.path(PATH_XVAL,paste0('phv',SNOW_VAR,'_pbias_',datestr,'.txt')))

}




