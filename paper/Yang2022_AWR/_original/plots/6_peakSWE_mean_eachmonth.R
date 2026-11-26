# for two LRMs 
tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
#write to file
path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/average_swe_Jan_Jun'
dir.create(path_out)
for(j in 1:6){
  datelist <- paste0(c(2004:2017),'0',j,'01')
  
  fns <- NA
  for(i in 1:length(datelist)){
    fn <- list.files('D:/Data/SWE/dataset/med/LRM/20200509_LRM_V1_pl_wofsca',glob2rx(paste0('*',datelist[i],'*.tif')), full.name=T)
    fns <- c(fns, fn)
  }
  fns <- fns[-1]
  r_stack <- stack(fns)
  for(i in 1:length(fns)){
    r_stack[[i]][r_stack[[i]]<0] <- 0
    r_stack[[i]][r_stack[[i]]>20] <- NA
  }
  rmean <- calc(r_stack, fun = mean, na.rm = T)
  r_baseline <- rmean
  
  fns <- NA
  for(i in 1:length(datelist)){
    fn <- list.files('D:/Data/SWE/dataset/med/LRM/LRM-FSCA_v1',glob2rx(paste0('*',datelist[i],'*.tif')), full.name=T)
    fns <- c(fns, fn)
  }
  fns <- fns[-1]
  r_stack <- stack(fns)
  for(i in 1:length(fns)){
    r_stack[[i]][r_stack[[i]]<0] <- 0
    r_stack[[i]][r_stack[[i]]>20] <- NA
  }
  rmean <- calc(r_stack, fun = mean, na.rm = T)
  r_fsca <- rmean
  
  fns <- NA
  for(i in 1:length(datelist)){
    fn <- list.files('D:/Data/SWE/dataset/ori/SNODAS/ori',glob2rx(paste0('*',datelist[i],'*.tif')), full.name=T)
    fns <- c(fns, fn)
  }
  fns <- fns[-1]
  r_stack <- stack(fns)
  for(i in 1:length(fns)){
    r_stack[[i]][r_stack[[i]]<0] <- 0
    r_stack[[i]][r_stack[[i]]>20000] <- NA
  }
  rmean <- calc(r_stack, fun = mean, na.rm = T)
  r_snodas <- rmean
  
  fns <- NA
  for(i in 1:length(datelist)){
    fn <- list.files('D:/Data/SWE/dataset/backup/NWM_SWE/Image_UTM500',glob2rx(paste0('*',datelist[i],'*.tif')), full.name=T)
    fns <- c(fns, fn)
  }
  fns <- fns[-1]
  r_stack <- stack(fns)
  for(i in 1:length(fns)){
    r_stack[[i]][r_stack[[i]]<0] <- 0
    r_stack[[i]][r_stack[[i]]>20000] <- NA
  }
  rmean <- calc(r_stack, fun = mean, na.rm = T)
  r_nwm_swe <- rmean
  
  
  writeRaster(r_baseline, paste0(path_out,'/2LRM_baseline_0',j,'.tif'), overwrite = T)
  writeRaster(r_fsca, paste0(path_out,'/1LRM_FSCA_0',j,'.tif'), overwrite = T)
  writeRaster(r_snodas, paste0(path_out,'/3SNODAS_0',j,'.tif'), overwrite = T)
  writeRaster(r_nwm_swe, paste0(path_out,'/4NWM_SWE_0',j,'.tif'), overwrite = T)
}

#plot average peak SWE along elevation------
library(StationSWERegressionV2)
dem <- raster('D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/SNM_DEM.tif')
if(F){
  fn <- list.files(path_out, '.tif', full.names = T)
  fnname <- list.files(path_out, '.tif', full.names = F)
  for(i in 1:length(fn)){
    reprojection_raster(fn[i], 'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/SNM_DEM.tif',
                        dir = 'D:/GoogleDrive/Research/P2_LRM_test/data/final/average_swe_Jan_Jun/resample', 
                        method = 'bilinear',
                        prefix = substr(fnname[i], 1, nchar(fnname[i])-4))
  }
}

# reproject data 
for(mi in 1:6){
  fn <- list.files(paste0(path_out, '/resample'), glob2rx(paste0('*0',mi, '*.tif')), full.names = T)
  rstack <- stack(fn)
  # change unit
  rstack[[1]] <- rstack[[1]] * 100
  rstack[[2]] <- rstack[[2]] * 100
  rstack[[3]] <- rstack[[3]] / 10
  rstack[[4]] <- rstack[[4]] / 10
  df <- cbind.data.frame(getValues(dem), getValues(rstack))
  names(df) <- c('Elevation', pid)
  head(df)
  summary(df)
  df <- df[complete.cases(df),]
  dim(df)
  
  # library(plyr)
  df$group <- floor((df$Elevation - 1499)/30)
  df2 <- df[df$Elevation >= 1500,]
  # a <- ddply(df, .(band=cut(Elevation, breaks = seq(1500,max(df$Elevation), by=30))), colwise(mean))
  
  library(outliers) #containing function outlier
  # library(dplyr)
  df3 <- data.frame()
  for(i in 1:4){
    df3 <- rbind(df3, cbind.data.frame(Elevation = df2$Elevation, value = df2[,i+1], id = names(df2)[i+1], group = df2$group))
  }
  dim(df3)
  library(mosaic)
  a <-
    df3 %>%
    group_by(group, id) %>%
    filter(!value %in% c(outlier(value))) %>%
    summarise(m = mean(value, na.rm = TRUE))
  
  a$Elevation <- a$group * 30 + 1500
  # a$volumne <- a$swe/1000*Area/1000
  head(a)
  
  colors <- c( '#7F9F00',"#FC4E07","#00AFBB",'#8106A9')
  lid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
  
  ggplot(a)+
    
    # geom_line(aes(x=Elevation, y = m, color = id), alpha=.8)+
    geom_smooth(aes(x=Elevation, y = m, color = id, fill = id), alpha=.3)+
    geom_point(aes(x=Elevation, y = m, color = id, shape = id), alpha=.8)+
    scale_color_manual(values=colors, labels=lid)+
    scale_fill_manual(values=colors, labels=lid)+
    scale_shape_manual(values=c(15,16,17,10), labels=lid)+
    theme_classic()+
    # ylim(0,80)+
    xlab('Elevation (m)')+
    ylab('14-year average peak SWE (cm)')+
    # theme_cowplot()+
    theme(legend.key=element_blank(),
          legend.text = element_text(size=11),
          legend.title = element_blank(),
          legend.position= c(0.15,0.8),
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          # legend.box.background = element_rect(fill = "transparent"),
          axis.title.x =element_text(size = axis_x_size,colour = "black"),
          axis.title.y = element_text(size=axis_y_size,colour = "black"),
          axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5,colour = "black"),
          axis.text.y = element_text(size = axis_laby_size,colour = "black"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.line = element_line(colour = "black"))
  # theme(plot.title = element_text(hjust = 0.5, vjust= -10, color='black'),
  #       axis.title = element_text(size = 13,color='black'),
  #       axis.text = element_text(size = 12,color = 'black'))
  
  outname <-  paste0('D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/ele_peakSWE_20200925_0',mi,'.jpg')
  ggsave(outname, width = 6, height=4, units='in',dpi = 600)
}

