
# validate five SWE products with ground observations, 9 pixel, mininum difference 
# kehan Yang 20200407


# 0 load libraries --------------------------------------------------------

library(lubridate)
library(StationSWERegressionV2)
#library(ggpubr)
library(dplyr)
#1. read station data -------------------------------------------------------

# sp_data <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/stationdata_20200315.txt')
# this is an updated version, removed negative, removed obs with 'e' flag
sp_data <- read.csv('/projects/keya6539/P2_LRM_test/scripts/0Pipelining/station_20200419.txt') 
# read all snow course data 
fn <- list.files('/projects/keya6539/P2_LRM_test/scripts/0Pipelining/snowcourse_ori','.txt', full.name=T)
sc_data <- NA
for(i in 1:length(fn)){
  data <- read.csv(fn[i])
  sc_data <- rbind(sc_data, data)
}
sc_data <- sc_data[-1,]

# relate fid to station ID
sc_info <- read.csv('/projects/keya6539/P2_LRM_test/scripts/0Pipelining/snowcourse_info.txt')
sp_info <- read.csv('/projects/keya6539/P2_LRM_test/scripts/0Pipelining/snowpillow_info.txt')
names(sc_info)[2] <- c('Site_ID')
names(sp_info)[2] <- c('Site_ID')

sc_data <- merge(sc_data, sc_info[,c('FID','Site_ID')], by.x = 'ID', by.y = 'Site_ID')
names(sc_data)[1] <- c('Site_ID')
sp_data <- merge(sp_data, sp_info[,c('FID','Site_ID')], by = 'Site_ID')


head(sp_data)
head(sc_data)
orisp_data <- sp_data
orisc_data <- sc_data


#2.1 write SC to one file ------------------------------------------------------------------
# each product, all available station-year
path_data <- '/scratch/summit/keya6539/Data/final/ground_ext'
path_out <- '/scratch/summit/keya6539/P2_LRM_test/stat/Extract/'

# -- need to check the location of the data --
i_start <- 1
i_end <- 10

if(T){
  # id <- c('LRM_V1_pl_wfscd','LRM_V1_pl_wfsca','LRM_V1_pl_wofsca')
  id <- c('20200609_LRM_FSCA','20200609_LRM_baseline','20200609_LRM_noREC')
  for(k in 1:length(id)){
    datelist <- sc_data$DateObs
    datelist <- unique(datelist)
    datelist <- datelist[order(datelist)]
    
    fn_ext <- list.files(paste0(path_data,'/',id[k]),glob2rx(paste0('*sc*.txt')), full.names = T)
    fn_ext <- fn_ext[substr(fn_ext, nchar(fn_ext)-14, nchar(fn_ext)-7) %in% as.character(datelist)]
    wdata_out <- NA
    
    for(i in 1:length(fn_ext)){
      datastr <- substr(fn_ext[i], nchar(fn_ext[i])-14, nchar(fn_ext[i])-7)
      data <- read.csv(fn_ext[i])
      
      sub_sc <- sc_data[sc_data$DateObs == as.numeric(datastr),]
      tpy <- merge(sub_sc, data[,i_start:(i_end+18)], by.x='FID', by.y='id')
      anst <- NA
      
      for(j in 1:nrow(tpy)){
        fsca <- tpy[j,(ncol(sub_sc)+i_start+18):(ncol(sub_sc)+i_end-1+18)]
        ori_swe <- tpy[j,(ncol(sub_sc)+i_start+9):(ncol(sub_sc)+i_end-1+9)]
        swe <- tpy[j,(ncol(sub_sc)+i_start):(ncol(sub_sc)+i_end-1)]
        
        if(sum(!is.na(swe)) > 0){
          nst_min <- swe[which.min(abs(swe-tpy[j,'SWE']))]
          i_nst <- which(as.vector(swe[1,]) == as.numeric(nst_min))
          nori_swe <- ori_swe[which.min(abs(ori_swe-tpy[j,'SWE']))]
          if(length(i_nst) > 1){
            if(5 %in% i_nst)
            {
              i_nst = 5
            }else
            {
              i_nst <- i_nst[1]
            }
            
          }
          nst <- swe[i_nst]
          nfsca <- fsca[i_nst]
          names(nfsca) <- 'fsca'
          names(nst) <- 'swe_scaled'
          names(nori_swe) <- 'swe_ori_ext'
          wdata <- cbind.data.frame(tpy[j,1:5], nst, nfsca,nori_swe)
          anst <- rbind(anst, wdata)
        }
      }
      
      if(sum(!is.na(anst))>0){
        anst <- anst[-1,]
        wdata_out <- rbind(wdata_out, anst)
      }
    }
    wdata_out <- wdata_out[!is.infinite(wdata_out$swe_scaled),]
    wdata_out <- wdata_out[!is.na(wdata_out$swe_scaled),]
    write.csv(wdata_out, paste0(path_out,id[k], '_sc.txt'), row.names = F)
  }
  
  # 2.2 Write SP to one file ------------------------------------------------------------------
  
  for(k in 1:length(id)){
    datelist <- sp_data$dte
    datelist <- unique(datelist)
    datelist <- datelist[order(datelist)]
    
    fn_ext <- list.files(paste0(path_data,'/',id[k]),glob2rx(paste0('*sp*.txt')), full.names = T)
    fn_ext <- fn_ext[substr(fn_ext, nchar(fn_ext)-14, nchar(fn_ext)-7) %in% as.character(datelist)]
    
    i_start <- 1
    i_end <- 10
    wdata_out <- NA
    for(i in 1:length(fn_ext)){
      datastr <- substr(fn_ext[i], nchar(fn_ext[i])-14, nchar(fn_ext[i])-7)
      data <- read.csv(fn_ext[i])
      
      sub_sc <- sp_data[sp_data$dte == as.numeric(datastr),]
      sub_sc$pillowswe <- sub_sc$pillowswe * 1000
      tpy <- merge(sub_sc, data[,i_start:(i_end+18)], by.x='FID', by.y='id')
      anst <- NA
      
      for(j in 1:nrow(tpy)){
        fsca <- tpy[j,(ncol(sub_sc)+i_start+18):(ncol(sub_sc)+i_end-1+18)]
        ori_swe <- tpy[j,(ncol(sub_sc)+i_start+9):(ncol(sub_sc)+i_end-1+9)]
        swe <- tpy[j,(ncol(sub_sc)+i_start):(ncol(sub_sc)+i_end-1)]
        
        if(sum(!is.na(swe)) > 0){
          nst_min <- swe[which.min(abs(swe - tpy[j,'pillowswe']))]
          i_nst <- which(as.vector(swe[1,]) == as.numeric(nst_min))
          nori_swe <- ori_swe[which.min(abs(ori_swe-tpy[j,'pillowswe']))]
          
          if(length(i_nst) > 1){
            if(5 %in% i_nst)
            {
              i_nst = 5
            }else
            {
              i_nst <- i_nst[1]
            }
          }
          nst <- swe[i_nst]
          nfsca <- fsca[i_nst]
          names(nfsca) <- 'fsca'
          names(nst) <- 'swe_scaled'
          names(nori_swe) <- 'swe_ori_ext'
          wdata <- cbind.data.frame(tpy[j,1:6], nst,nfsca,nori_swe)
          anst <- rbind(anst, wdata)
        }
      }
      if(sum(!is.na(anst))>0){
        anst <- anst[-1,]
        wdata_out <- rbind(wdata_out, anst)
      }
    }
    wdata_out <- wdata_out[!is.infinite(wdata_out$swe_scaled),]
    wdata_out <- wdata_out[!is.na(wdata_out$swe_scaled),]
    write.csv(wdata_out, paste0(path_out,id[k], '_sp.txt'), row.names = F)
  }
}

