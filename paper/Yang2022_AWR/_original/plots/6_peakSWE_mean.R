# 0.load libraries --------------------------------------------------------

library(ggplot2)
library(raster)
library(mosaic)
library(forcats)
library(stationsweRegression)
dfalpha <- 0.1
dfsize <- 250
axis_x_size <- 13
axis_y_size <- 13
axis_labx_size <- 12
axis_laby_size <- 12
legend_size <- 4.2


fn <- list.files('D:/GoogleDrive/Research/P2_LRM_test/Stat/streamflow/sierramean','.txt', full.name=T)
# fn <- list.files('/scratch/summit/keya6539/P2_LRM_test/stat/SierraMean','.txt', full.name=T)
fn <- fn[-3]
ids <- c('LRM-FSCA','LRM-baseline','NWM-SWE','SNODAS')
pdata <- data.frame()
for(i in 1:length(fn)){
  data <- read.csv(fn[i])
  pdata <- rbind(pdata, cbind.data.frame(date = data$dates, meanSWE = data$SM_mean, id = ids[i]))
}
pdata$Date <- as.Date(as.character(pdata$date), '%Y%m%d')
subpdata <- pdata[pdata$Date >= as.Date('20031001','%Y%m%d'),]
subpdata <- subpdata[subpdata$Date < as.Date('20171001','%Y%m%d'),]
#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(subpdata$meanSWE, 3,fill = list(NA, NULL, NA))

#Add calculated moving averages to existing data frame
subpdata$amb.av=coredata(m.av)
ggplot(subpdata)+
  # geom_line(aes(x = Date, y = amb.av, color = id))+
  geom_line(aes(x = Date, y = meanSWE, color = id))+
  geom_point(aes(x = Date, y = meanSWE, color = id))+
  theme_bw()


# wdata$WY[wdata$id == 'NWM'] <- wdata$WY[wdata$id == 'NWM']-1
colors <- c('#7F9F00',"#FC4E07", "#00AFBB",'#8106A9')
pid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
wdata <- subpdata
wdata$porder <- NA
wdata$porder[wdata$id=='LRM-baseline'] <- 2
wdata$porder[wdata$id=='LRM-FSCA'] <- 1
wdata$porder[wdata$id=='NWM-SWE'] <- 4
wdata$porder[wdata$id=='SNODAS'] <- 3

tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
# tem <- raster('/scratch/summit/keya6539/P2_LRM_test/stat/SierraMean/SNM_500.tif')
Npixel <- sum(!is.na(tem[]))
Area <- Npixel*500*500/1000/1000
wdata$volume <- wdata$meanSWE*Area/1000/1000
# remove LRM-noREC
wdata %>%
  mutate(pid = fct_reorder(id, porder)) %>%
  ggplot()+
  # geom_smooth(aes(x=WY, y=Volume,group=pid,color = pid, fill = pid),level = 0.95, alpha=0.1, method = 'lm')+
  annotate("rect", xmin=as.Date('20111001','%Y%m%d'), xmax=as.Date('20160930','%Y%m%d'), ymin = -Inf, ymax = Inf,fill="gray50", alpha=0.4) +
  annotate("text", x=as.Date('20140101','%Y%m%d'), y=40 ,label = "2012-2016 drought", size = 5) +
  # geom_point(aes(x=Date, y=volume, color = pid, shape = pid), show.legend =T, alpha=0.3, size=2)+
  # ylim(c(0,80))+
  geom_line(aes(x=Date, y=volume, color = pid), show.legend =T, alpha=0.8, size=1)+
  scale_color_manual(values=colors, labels=pid)+
  # scale_fill_manual(values=colors, labels=pid)+
  scale_shape_manual(values=c(15,16,17,10,7), labels=pid)+
  ylab('Sierra-wide snow water storage (Gt)')+
  xlab('Date')+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  # ylim(c(ymins[i], ymaxs[i]))+
  # scale_fill_manual(values=colors, labels=pid)+    
  # scale_color_manual(values=colors, labels=pid)+
  theme_bw()+
  theme(legend.text = element_text(size=11),
        legend.position = c(0.1,0.87),
        legend.title = element_blank(),
        legend.background = element_blank())
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/average_snow_storage_line_v2.jpg'
# outname <-  '/scratch/summit/keya6539/P2_LRM_test/stat/SierraMean/average_snow_storage_line_v2.tiff'
# outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/1_cross_validation_legend_dpi600.jpg'
ggsave(outname, width = 10, height=4, units='in',dpi = 600)



# peak SWE and Peak SWE date ----------------------------------------------
library(lfstat)
wdata$WY <- water_year(wdata$Date, origin = 10)

pwdata <- wdata %>%
  group_by(WY, id) %>%
  summarise(peakv = volume[which.max(volume)], peakdate = date[which.max(volume)])

pwdata$porder <- NA
pwdata$porder[pwdata$id=='LRM-baseline'] <- 2
pwdata$porder[pwdata$id=='LRM-FSCA'] <- 1
pwdata$porder[pwdata$id=='NWM-SWE'] <- 4
pwdata$porder[pwdata$id=='SNODAS'] <- 3
pwdata <- as.data.frame(pwdata)
pwdata$WY <- as.numeric(as.character(pwdata$WY))

a <- pwdata %>%
  group_by(WY) %>%
  summarise(peakvm = mean(peakv), sd = sd(peakv))


if(T){
  
  f1<-
    pwdata %>%
    mutate(pid = fct_reorder(id, porder)) %>%
    ggplot()+
    # geom_smooth(aes(x=WY, y=Volume,group=pid,color = pid, fill = pid),level = 0.95, alpha=0.1, method = 'lm')+
    annotate("rect", xmin=2011.5, xmax=2016.5, ymin = -Inf, ymax = Inf,fill="gray50", alpha=0.4) +
    annotate("text", x=2014, y=50 ,label = "2012-2016 drought") +
    geom_point(aes(x=WY, y=peakv, color = pid, shape = pid), show.legend =F, alpha=0.8, size=2)+
    # ylim(c(0,80))+
    geom_line(aes(x=WY, y=peakv, color = pid), show.legend =F, alpha=0.8, size=0.5)+
    scale_color_manual(values=colors, labels=pid)+
    # scale_fill_manual(values=colors, labels=pid)+
    scale_shape_manual(values=c(15,16,17,10,7), labels=pid)+
    ylab('Maximum SWS (Gt)')+
    xlab('Water Year')+
    ylim(c(0,60))+
    scale_x_continuous(breaks = seq(min(pwdata$WY), max(pwdata$WY), by = 2))+
    # ylim(c(ymins[i], ymaxs[i]))+
    # scale_fill_manual(values=colors, labels=pid)+    
    # scale_color_manual(values=colors, labels=pid)+
    theme_cowplot()+
    theme(legend.text = element_text(size=9),
          legend.position = c(0.1,0.82),
          legend.title = element_blank(),
          legend.background = element_blank())
  # outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage.jpg'
  # outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/1_cross_validation_legend_dpi600.jpg'
  # ggsave(outname, width = 8, height=4, units='in',dpi = 600)
  
  
  pwdata$doy <- as.numeric(strftime(as.Date(as.character(pwdata$peakdate),'%Y%m%d'),'%j'))
  # 2014 NWM 
  for(i in 1:nrow(pwdata)){
    if(pwdata$doy[i] > 200){
      pwdata$doy[i] = pwdata$doy[i] - 365
    }
  }
  f2<-
    pwdata %>%
    mutate(pid = fct_reorder(id, porder)) %>%
    ggplot()+
    # geom_smooth(aes(x=WY, y=Volume,group=pid,color = pid, fill = pid),level = 0.95, alpha=0.1, method = 'lm')+
    annotate("rect", xmin=2011.5, xmax=2016.5, ymin = -Inf, ymax = Inf,fill="gray50", alpha=0.4) +
    # annotate("text", x=2014, y=60 ,label = "2012-2016 drought") +
    geom_point(aes(x=WY, y=doy, color = pid, shape = pid), show.legend =T, alpha=0.8, size=2)+
    # ylim(c(0,80))+
    geom_line(aes(x=WY, y=doy, color = pid), show.legend =T, alpha=0.8, size=0.5)+
    # geom_smooth(aes(x=WY, y=doy, fill = pid), method = 'loess',show.legend =T, alpha=0.2)
    scale_color_manual(values=colors, labels=pid)+
    # scale_fill_manual(values=colors, labels=pid)+
    scale_shape_manual(values=c(15,16,17,10), labels=pid)+
    ylab('Day of year')+
    xlab('Water Year')+
    ylim(c(-10,120))+
    scale_x_continuous(breaks = seq(min(pwdata$WY), max(pwdata$WY), by = 2))+
    # ylim(c(ymins[i], ymaxs[i]))+
    # scale_fill_manual(values=colors, labels=pid)+
    # scale_color_manual(values=colors, labels=pid)+
    # theme_bw()+
    theme_cowplot()+
    theme(legend.text = element_text(size=9),
          legend.position = c(0.05,0.3),
          legend.title = element_blank(),
          legend.background = element_blank())
  
  
  
  medians <- aggregate(doy ~  id, pwdata, median)
  medians_v <- aggregate(peakv ~  id, pwdata, median)
  medians$doy <- round(medians$doy, 0)
  medians_v$peakv <- format(round(medians_v$peakv,1), nsmall = 1)
  f4<-pwdata %>%
    mutate(pid = fct_reorder(id, porder)) %>%
    ggplot()+
    geom_boxplot(aes(x=pid, y=doy, color = pid), show.legend =F, alpha=0.8, size=0.5)+
    geom_text(data = medians, aes(x = id, label = paste0(doy,' DOY'), y = 120))+
    scale_color_manual(values=colors, labels=pid)+
    ylab('Day of year')+
    xlab('Datasets')+
    ylim(c(-10,120))+
    theme_cowplot()+
    theme(legend.text = element_text(size=9),
          legend.position = c(0.2,0.82),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text.x = element_text(angle = 0),
          axis.title.x = element_blank())
  
  f3<-pwdata %>%
    mutate(pid = fct_reorder(id, porder)) %>%
    ggplot()+
    geom_boxplot(aes(x=pid, y=peakv, color = pid), show.legend =F, alpha=0.8, size=0.5)+
    geom_text(data = medians_v, aes(x = id, label = paste0(as.character(medians_v$peakv),' Gt'), y = 60))+
    scale_color_manual(values=colors, labels=pid)+
    ylab('Maximum SWS (Gt)')+
    xlab('Datasets')+
    ylim(c(0,60))+
    theme_cowplot()+
    theme(legend.text = element_text(size=9),
          legend.position = c(0.2,0.82),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text.x = element_text(angle = 0),
          axis.title.x = element_blank())
}

library(ggpubr)
ggarrange(f1+rremove("xlab")+rremove('x.text'),
          f3+rremove("xlab")+rremove('x.text')+rremove("ylab")+rremove('y.text'),
          f2,
          f4+rremove("ylab")+rremove('y.text'),
          align = 'hv',
          labels = c("A", "a",'B','b'), 
          vjust = 3,
          hjust = -7,
          widths = c(7,4),
          heights = c(5,5),
          nrow = 2,ncol = 2)

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_date.jpg'
# outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/1_cross_validation_legend_dpi600.jpg'
ggsave(outname, width = 10, height=5, units='in',dpi = 600)



# get peak SWE data for maps ----------------------------------------------
# for two LRMs 
tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
if(T){
  datelist <- pwdata$peakdate[pwdata$id == 'LRM-baseline']
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
  
  datelist <- pwdata$peakdate[pwdata$id == 'LRM-FSCA']
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
  
  datelist <- pwdata$peakdate[pwdata$id == 'SNODAS']
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
  
  datelist <- pwdata$peakdate[pwdata$id == 'NWM-SWE']
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
  
  #write to file
  path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/peak_mean2014_2017'
  writeRaster(r_baseline, paste0(path_out,'/2LRM_baseline.tif'), overwrite = T)
  writeRaster(r_fsca, paste0(path_out,'/1LRM_FSCA.tif'), overwrite = T)
  writeRaster(r_snodas, paste0(path_out,'/3SNODAS.tif'), overwrite = T)
  writeRaster(r_nwm_swe, paste0(path_out,'/4NWM_SWE.tif'), overwrite = T)
}

#plot average peak SWE along elevation------
library(StationSWERegressionV2)
dem <- raster('D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/SNM_DEM.tif')
if(F){
  fn <- list.files(path_out, '.tif', full.names = T)
  fnname <- list.files(path_out, '.tif', full.names = F)
  for(i in 1:length(fn)){
    reprojection_raster(fn[i], 'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/SNM_DEM.tif',
                        dir = 'D:/GoogleDrive/Research/P2_LRM_test/data/final/peak_mean2014_2017/resample', 
                        method = 'bilinear',
                        prefix = substr(fnname[i], 1, nchar(fnname[i])-4))
  }
}

# reproject data 

########################## plot elevation v.s. SWS ##################################
dem <- raster('D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/SNM_DEM.tif')
fn <- list.files(paste0(path_out, '/resample'), '.tif', full.names = T)
fn <- list.files('D:/GoogleDrive/Research/P2_LRM_test/data/final/pixel_wise_peak_mean/plot', '.tif', full.names = T)
rstack <- stack(fn)
# tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500_WM.tif')
# remove water mask
pid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
rstack <- rstack * tem
dem <- dem *tem
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
N_inter <- 5
df$group <- floor((df$Elevation - 1499)/N_inter)
df2 <- df[df$Elevation >= 1500,]
# a <- ddply(df, .(band=cut(Elevation, breaks = seq(1500,max(df$Elevation), by=30))), colwise(mean))

library(outliers) #containing function outlier
# library(dplyr)
df3 <- data.frame()
for(i in 1:4){
  df3 <- rbind(df3, cbind.data.frame(Elevation = df2$Elevation, value = df2[,i+1], id = names(df2)[i+1], group = df2$group))
}
dim(df3)
head(df3)
# library(mosaic)
a <-
  df3 %>%
  group_by(group, id) %>%
  filter(!value %in% c(outlier(value))) %>%
  summarise(m = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), sum_pixel = sum(!is.na(value)))

a$Elevation <- a$group * N_inter + 1500
a$area <- a$sum_pixel * 0.5 * 0.5 #unit km2
# a$volumne <- a$swe/1000*Area/1000
head(a)

colors <- c( '#7F9F00',"#FC4E07","#00AFBB",'#8106A9')
lid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
ggplot(a)+
  geom_point(aes(x=Elevation, y = sum_pixel), alpha=.8)
# low elevations < 2500

1-sum(a$area[a$Elevation < 2500])/sum(a$area)

  
ggplot(a)+
  
  # geom_line(aes(x=Elevation, y = m, color = id), alpha=.8)+
  # geom_smooth(aes(x=Elevation, y = m, color = id, fill = id), alpha=.3)+
  geom_ribbon(aes(x = Elevation, ymin=m-sd, ymax=m+sd, fill = id), alpha = 0.2) +
  geom_point(aes(x=Elevation, y = area/(max(a$area)/max((a$m+35)))), alpha=.6, color = 'red')+ # max m = 108 + 35 to ~150 
  geom_point(aes(x=Elevation, y = m, color = id, shape = id), alpha=.8)+
  
  scale_y_continuous(name = '14-year average pixel-wise peak SWE (cm)', # first axis
                     sec.axis = sec_axis(~ .*(max(a$area)/max((a$m+35))), name = expression(paste(Area (km ^2)))))+
  
  # scale_y_continuous(
  #   
  #   # Features of the first axis
  #   name = "First Axis",
  #   
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~.*coeff, name="Second Axis")
  # )

  scale_color_manual(values=colors, labels=lid)+
  scale_fill_manual(values=colors, labels=lid)+
  scale_shape_manual(values=c(15,16,17,10), labels=lid)+
  scale_x_continuous(breaks = seq(1500, 4200, by = 300))+
  theme_classic()+
  # ylim(0,80)+
  xlab('Elevation (m)')+
  # ylab('14-year average peak SWE (cm)')+
  # ylab('14-year average pixel-wise peak SWE (cm)')+
  # theme_cowplot()+
  theme(legend.key=element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position= c(0.3,0.85),
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
        axis.line = element_line(colour = "black"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right=element_text(colour='red'))
# theme(plot.title = element_text(hjust = 0.5, vjust= -10, color='black'),
#       axis.title = element_text(size = 13,color='black'),
#       axis.text = element_text(size = 12,color = 'black'))

# outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/ele_peakSWE_20200925_v4.jpg
path_fig <- 'D:/GoogleDrive/Research/P2_LRM_test/0submittion/figures'
# outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/ele_pixel_wise_peakSWE_20210128_v5_WM.jpg'
outname <-  paste0(path_fig,'/ele_pixel_wise_peakSWE_20210128_v5_WM.jpg')
ggsave(outname, width = 10, height=5, units='in',dpi = 600)




ggplot(a[a$id == c('SNODAS','NWM-SWE'),])+
  
  # geom_line(aes(x=Elevation, y = m, color = id), alpha=.8)+
  # geom_smooth(aes(x=Elevation, y = m, color = id, fill = id), alpha=.3)+
  geom_point(aes(x=Elevation, y = m, color = id, shape = id), alpha=.8)+
  scale_x_continuous(breaks = seq(1500, 4200, by = 50))

# plot average peak maps --------------------------------------------------
library(rasterVis) 
library(viridis)
cols <- colorRampPalette(brewer.pal(9,"Blues"))
# pid<-c('LRM-FSCA','LRM-baseline','LRM-noREC','SNODAS','NWM-SWE')
pid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')

color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
x <- c((1:100)^2, (100:1)^2)
plot(x,col=color.gradient(x), pch=19,cex=2)

# colfunc<-colorRampPalette(c("yellow","green","blues"))
# colfunc<-colorRampPalette('Spectral')
# plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)


fn <- list.files(paste0(path_out, '/resample'), '.tif', full.names = T)
rstack <- stack(fn)
tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
rstack <- rstack*tem
# change unit
rstack[[1]] <- rstack[[1]] * 100
rstack[[2]] <- rstack[[2]] * 100
rstack[[3]] <- rstack[[3]] / 10
rstack[[4]] <- rstack[[4]] / 10
rdiff <- rstack
rdiff[[1]] <- rstack[[1]] - rstack[[2]]
rdiff[[2]] <- rstack[[1]] - rstack[[3]]
rdiff[[3]] <- rstack[[1]] - rstack[[4]]
rdiff[[4]] <- rstack[[2]] - rstack[[3]]
rdiff[[5]] <- rstack[[2]] - rstack[[4]]
rdiff[[6]] <- rstack[[3]] - rstack[[4]]


N=160
my.at <- seq(0, N, by = 5)
# my.at[length(my.at)+1] <- 600
my.col <- topo.colors(length(my.at))
my.col <- rev(my.col)

rstack2 <- rstack
for(i in 1:4){
  rstack2[[i]][rstack2[[i]]>N] <- N
}


p1<-
  levelplot(rstack2,
          # main="Landsat NDVI/nNEON Harvard Forest",
          # col.regions=my.col, 
          # col.regions=rev(colfunc(length(my.at))),   
          layout=c(4, 1),
          col.regions=rev(viridis(length(my.at))), 
          at=my.at, 
          names.attr=pid,
          colorkey=list(space="bottom",labels=list(cex=1, font=2, col="black"),
                        height=0.7, width=1),
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))

N2 <- 40
my.at <- seq((-1)*N2, N2, by = 5)
my.col <- colorRamps::matlab.like(n=length(my.at))
my.col <- rev(my.col)
rdiff2 <- rdiff
for(i in 1:6){
  rdiff2[[i]][rdiff2[[i]]>N2] <- N2
  rdiff2[[i]][rdiff2[[i]]< (-1)*N2] <- (-1)*N2
}

p2<-
  levelplot(rdiff2,
          # main="Landsat NDVI/nNEON Harvard Forest",
          col.regions=my.col,
          # col.regions=rev(colfunc(length(my.at))),
          layout=c(3, 2),
          # col.regions=rev(viridis(length(my.at))),
          at=my.at,
          # names.attr=pid,
          colorkey=list(space="bottom",labels=list(cex=1, font=2, col="black"),
                        height=0.7, width=1),
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))


outname <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_maps3_v3.jpg'
jpeg(outname,width = 12, height=5, units='in',res = 2500)
print(p1)
dev.off()

outname <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_maps3_diff_v4.jpg'
jpeg(outname,width = 8, height=10, units='in',res = 2500)
print(p2)
dev.off()


# compare with dem
df_diff <- cbind.data.frame(getValues(dem), getValues(rdiff))
df_diff <- df_diff[complete.cases(df_diff),]
res1 <- lm(df_diff$`getValues(dem)` ~ df_diff$layer.2.1)
summary(res1)


averagediff <- calc(rstack, fun = mean, na.rm = T)

# relative difference, the mantude of the average difference between a pair of datasets at a grid cell, relative to 
# an estimate of the average precipitation at that grid cee -- Henn et al 2018
plot(rdiff)
pdiff <- rstack
pdiff[[1]] <- (pdiff[[1]] - pdiff[[2]])/averagediff
pdiff[[2]] <- (pdiff[[1]] - pdiff[[3]])/averagediff
pdiff[[3]] <- (pdiff[[1]] - pdiff[[4]])/averagediff
pdiff[[4]] <- (pdiff[[2]] - pdiff[[3]])/averagediff
pdiff[[5]] <- (pdiff[[2]] - pdiff[[4]])/averagediff
pdiff[[6]] <- (pdiff[[3]] - pdiff[[4]])/averagediff
plot(pdiff)

my.at <- seq(0, 100, by = 5)
my.col <- topo.colors(length(my.at))
my.col <- rev(my.col)

levelplot(abs(pdiff*100),
          # main="Landsat NDVI/nNEON Harvard Forest",
          col.regions=my.col,
          # col.regions=rev(colfunc(length(my.at))),
          layout=c(3, 2),
          # col.regions=rev(viridis(length(my.at))),
          at=my.at,
          # names.attr=pid,
          colorkey=list(space="bottom",labels=list(cex=0.8, font=4, col="black"),
                        height=0.7, width=1),
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
