# extract swe for snow course and snow pillows -----------------------------------
# Kehan Yang 2020-03-24
# WGS 100
# data includes: Margulis SWE, FSCA

library(raster)
library(doParallel)
library(foreach)



# read template 
tag <- '202000904_DMCFSCA'
if(F){
  source('D:/GoogleDrive/Github/R/0usefulltools/usefulltools/reprojection_raster.R')
  path_data <- 'D:/Data/SWE/dataset/med/LRM/20200418_LRM_V1_pl_wfsca'
  
  path_tem <- 'D:/GoogleDrive/Research/P2_LRM_test/data/template/'
  path_fsca <- 'D:/Data/SWE/dataset/ori/FSCAWUS'
  
  path_out <- paste0('D:/GoogleDrive/Research/P2_LRM_test/data/med/extract/', tag)
  path_del <- paste0('D:/GoogleDrive/Research/P2_LRM_test/data/del/', tag)
  path_del_resa <- paste0('D:/GoogleDrive/Research/P2_LRM_test/data/del/LRM_UTM500')
}

# path_data <- paste0('/scratch/summit/keya6539/P2_LRM_test/output/',tag)
path_data <- '/scratch/summit/keya6539/P6_2pages_SWEsampling/data/modeltest/input/dynamicvars/DMFSCA'
path_tem <- '/scratch/summit/keya6539/Data/template/'
# path_fsca <- '/scratch/summit/keya6539/Data/med/FSCA_LRM_WGS'
path_out <- paste0('/scratch/summit/keya6539/Data/final/ground_ext/', tag)
# path_del <- paste0('/scratch/summit/keya6539/Data/med/fsca_del/', tag,'500')
# path_del_resa <- paste0('/scratch/summit/keya6539/Data/med/LRM_UTM500')

dir.create(path_out)
# dir.create(path_del)
# dir.create(path_del_resa)


tem_sc <- raster(paste0(path_tem,'station/SC_500_WGS.tif'))
tem_sp <- raster(paste0(path_tem,'station/SP_500_WGS.tif'))
# tem <- raster(paste0(path_tem,'SNM_tem/snm_watermask.tif'))
ftemp <- paste0(path_tem,'SNM_tem/snm_watermask.tif')

m_sc <- raster::as.matrix(tem_sc)
m_sp <- raster::as.matrix(tem_sp)


fn <- list.files(path_data,'.tif$', full.names = T, recursive = T)
# start from 2000-03-01 because fsca data are only available after 2000 
fn <- fn[as.numeric(substr(fn, nchar(fn)-11, nchar(fn)-4)) >= 20000301]

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i = 1:length(fn),
        .packages = c('raster'),
        .export = c('path_tem','path_data','path_out','ftemp','m_sc','m_sp','fn'))%dopar%{
          
          
          source('/projects/keya6539/P2_LRM_test/scripts/0functions/reprojection_raster.R')
          get_nbr <- function(m_r, m_tem){
            v1 <- m_r[which(!is.na(m_tem)) - nrow(m_tem) - 1]
            v2 <- m_r[which(!is.na(m_tem)) - 1]
            v3 <- m_r[which(!is.na(m_tem)) + nrow(m_tem) - 1]
            
            v4 <- m_r[which(!is.na(m_tem)) - nrow(m_tem)]
            v5 <- m_r[which(!is.na(m_tem))]
            v6 <- m_r[which(!is.na(m_tem)) + nrow(m_tem)]
            
            v7 <- m_r[which(!is.na(m_tem)) - nrow(m_tem) + 1]
            v8 <- m_r[which(!is.na(m_tem)) + 1]
            v9 <- m_r[which(!is.na(m_tem)) + nrow(m_tem) + 1]
            
            df <- data.frame(v1,v2,v3,v4,v5,v6,v7,v8,v9)
            return(df)
          }
          
          datestr <- substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4)
          outname_sc <- paste0(path_out,'/',datestr,'_sc.txt')
          outname_sp <- paste0(path_out,'/',datestr,'_sp.txt')
          
          if(!file.exists(outname_sc)){
            
            # r_resa <- reprojection_raster(fn[i], ftemp , dir = path_del_resa, method = 'bilinear', prefix = paste0('LRM_WGS500_', datestr))
            r <- raster(fn[i])
            # r[r<0] <- NA
            # r[r>20] <- NA
            # r <- r * 1000
            m_r <- raster::as.matrix(r)
            
            # get snow course values for 9 neighbors 
            # m_r and m_tem
            df_sc <- get_nbr(m_r, m_sc)
            df_sc <- cbind.data.frame(id = m_sc[which(!is.na(m_sc))], df_sc)
            df_sc <- df_sc[order(df_sc$id),]
            
            df_sp <- get_nbr(m_r, m_sp)
            df_sp <- cbind.data.frame(id = m_sp[which(!is.na(m_sp))], df_sp)
            df_sp <- df_sp[order(df_sp$id),]
            
            # resample fsca and extract fsca
            # fsca <- list.files(path_fsca, glob2rx(paste0('*',datestr,'*.tif')), full.names = T, recursive = T)
            # fsca_resa <- reprojection_raster(fimage = fsca, ftemp = ftemp, dir = path_del, method = 'bilinear', prefix = paste0(datestr,'_fsca'))
            # m_fsca <- raster::as.matrix(raster(fsca))
            
            # df_fsca_sp <- get_nbr(m_fsca, m_sp)
            # df_fsca_sp <- cbind.data.frame(id = m_sp[which(!is.na(m_sp))], df_fsca_sp)
            # df_fsca_sp <- df_fsca_sp[order(df_fsca_sp$id),]
            # names(df_fsca_sp) <- c('id',paste0('f',1:9))
            # 
            # df_fsca_sc <- get_nbr(m_fsca, m_sc)
            # df_fsca_sc <- cbind.data.frame(id = m_sc[which(!is.na(m_sc))], df_fsca_sc)
            # df_fsca_sc <- df_fsca_sc[order(df_fsca_sc$id),]
            # names(df_fsca_sc) <- c('id',paste0('f',1:9))
            # 
            # calculate station swe, gridswe / fsca == station swe
            # df_swe_sc <- round(df_sc / df_fsca_sc * 100, 0) 
            # df_swe_sc$id <- df_sc$id
            # names(df_swe_sc) <- c('id',paste0('swe',1:9))
            # 
            # df_swe_sp <- round(df_sp / df_fsca_sp * 100, 0) 
            # df_swe_sp$id <- df_sp$id
            # names(df_swe_sp) <- c('id',paste0('swe',1:9))
            # 
            # output
            # v1-v9, the extract value
            # swe1-swe9, scaled by fsca
            
            # wsc <- merge(df_sc, df_s, by='id')
            # wsc <- merge(df_swe_sc, wsc, by = 'id')
            # wsp <- merge(df_swe_sp, merge(df_sp, df_fsca_sp, by='id'), by='id')
            # head(wsc)
            # head(wsp)
            
            write.csv(df_sc,outname_sc,row.names = F)
            write.csv(df_sp,outname_sp,row.names = F)
          }
        }

stopCluster(cl)


# path <- '/scratch/summit/keya6539/Data/final/ground_ext/202000904_DMCFSCA'
# fn <- list.files(path, glob2rx('*sp*.txt'), full.names = T)
# fnname <- list.files(path, glob2rx('*sp*.txt'), full.names = F)
# all <- data.frame()
# for(i in 1:length(fn)){
#   data <- read.csv(fn[i])
#   all <- rbind(all,data)
# }
# all$date <- substr(fnname, 1, 8)
# write.csv(all,'/scratch/summit/keya6539/Data/final/ground_ext/202000904_DMCFSCA_sp.txt')
