library(rasterVis)
library(ggpubr)
library(colorRamps)
if(F){
  path <- '/scratch/summit/keya6539/P2_LRM_test/Data/final/peak'
  # path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/peak'
  pathout <- '/scratch/summit/keya6539/P2_LRM_test/Data/final/peak_mean'
  fn <- list.files(path, '.tif', full.names = T)
  outlist <- c('LRM_FSCA','LRM_baseline','LRM_noREC','NWM_SWE','SNODAS') 
  outlist <- c(paste0('PeakDate_', outlist), paste0('PeakSWE_', outlist))
  # 2004-2017 average 
  fn <- fn[substr(fn, nchar(fn)-7, nchar(fn)-4) %in% c(as.character(2004:2017))]
  for(i in 2:10){
    index1 <- 14*(i-1)+1
    index2 <- 14*(i)
    subfn <- fn[index1:index2]
    outname <- strsplit(subfn[1],'/')
    outname <- outname[[1]]
    outname <- outname[length(outname)]
    print(outname)
    
    r <- stack(subfn)
    rmean <-  calc(r, fun = mean, na.rm = T)
    outname <-paste0(pathout,'/',outlist[i],'.tif')
    writeRaster(rmean, outname, overwrite = T)
  }
}

colors <- c('#7F9F00',"#FC4E07", "#00AFBB",'#8106A9')
dfalpha <- 0.7
dfsize <- 250
axis_x_size <- 13
axis_y_size <- 13
axis_labx_size <- 12
axis_laby_size <- 12
legend_size <- 4.2

if(T){
  
  path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/pixel_wise_peak_mean'
  fn <- list.files(path, glob2rx('*PeakSWE*.tif'), full.names = T)
  fnname <- list.files(path, glob2rx('*PeakSWE*.tif'), full.names = F)
  # remove noREc
  fn <- fn[-3]
  fnname <- fnname[-3]
  # tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
  tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500_WM.tif')
  rstack <- stack(fn)
  rstack <- rstack*tem
  
  rstack[[1]] <- rstack[[1]]*100
  rstack[[2]] <- rstack[[2]]*100
  rstack[[3]] <- rstack[[3]]/10
  rstack[[4]] <- rstack[[4]]/10
  # rstack[[5]] <- rstack[[5]]/10
  
  
  library(rasterVis) 
  cols <- colorRampPalette(brewer.pal(9,"Blues"))
  fnname
  # pid<-c('LRM-FSCA','LRM-baseline','LRM-noREC','SNODAS','NWM-SWE')
  pid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
  
  color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
  }
  x <- c((1:100)^2, (100:1)^2)
  plot(x,col=color.gradient(x), pch=19,cex=2)
  
  colfunc<-colorRampPalette(c("yellow","green","blue"))
  # colfunc<-colorRampPalette('Spectral')
  plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)
  N=200
  my.at <- seq(0, N, by = 10)
  # my.at[length(my.at)+1] <- 600
  my.col <- topo.colors(length(my.at))
  my.col <- rev(my.col)
  for(i in 1:4){
    rstack[[i]][rstack[[i]]>N] <- N
  }
  p <-
    levelplot(rstack,
              # main="Landsat NDVI/nNEON Harvard Forest",
              # col.regions=my.col, 
              # col.regions=rev(colfunc(length(my.at))),   
              layout=c(4, 1),
              col.regions=rev(viridis(length(my.at))), 
              at=my.at, 
              names.attr=pid,
              colorkey=list(space="bottom",labels=list(cex=0.8, font=2, col="black"),
                            height=0.7, width=1),
              xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
  
  path_figout <- 'D:/GoogleDrive/Research/P2_LRM_test/0submittion/figures'
  outname <- paste0(path_figout,'/peak_snow_storage_maps3_v.jpg')
  jpeg(outname,width = 12, height=5, units='in',res = 2500)
  print(p)
  dev.off()
  
}


# outname <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/0_allinone/peak_snow_storage_maps3_v2.jpg'
# jpeg(outname,width = 12, height=5, units='in',res = 2500)
# print(p)
# dev.off()


# plot anormal
path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/pixel_wise_peak_mean'
fn <- list.files(path, glob2rx('*PeakSWE*.tif'), full.names = T)
fnname <- list.files(path, glob2rx('*PeakSWE*.tif'), full.names = F)
# remove noREc
fn <- fn[-3]
fnname <- fnname[-3]
tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
rstack <- stack(fn)
rstack <- rstack*tem

rstack[[1]] <- rstack[[1]]*100
rstack[[2]] <- rstack[[2]]*100
rstack[[3]] <- rstack[[3]]/10
rstack[[4]] <- rstack[[4]]/10
rmean <- mean(rstack)

rabnormal <- rstack
rabnormal[[1]] <- rstack[[1]] - rmean
rabnormal[[2]] <- rstack[[2]] - rmean
rabnormal[[3]] <- rstack[[3]] - rmean
rabnormal[[4]] <- rstack[[4]] - rmean
plot(rabnormal)

N = 30
rabnormal[[1]][rabnormal[[1]][]>N]<- N
rabnormal[[2]][rabnormal[[2]][]>N]<- N
rabnormal[[3]][rabnormal[[3]][]>N]<- N
rabnormal[[4]][rabnormal[[4]][]>N]<- N

nint=160
# my.at <- seq(-240, N, by = 10)  # 
my.at <- seq(-N,N,
             length.out = nint + 1)

p<-levelplot(rabnormal,
             # main="Landsat NDVI/nNEON Harvard Forest",
             # col.regions=my.col, 
             # col.regions=rev(colfunc(length(my.at))),   
             layout=c(4, 1),
             col.regions= colorRampPalette(c('#FF0000','white','#000099')),#col.regions=rev(viridis(length(my.at))), 
             at=my.at, 
             names.attr=pid,
             colorkey=list(space="bottom",labels=list(cex=0.8, font=2, col="black"),
                           height=0.7, width=1),
             xlab=NULL, ylab=NULL, scales=list(draw=FALSE))

# outname <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_maps3_v2_abnormal.jpg'
outname <- paste0(path_figout,'/peak_snow_storage_maps3_v2_abnormal.jpg')
jpeg(outname,width = 12, height=5, units='in',res = 2500)
print(p)
dev.off()

# for dissertation -----------------------------
my.breaks <- seq(-300, 300, by=20)
my.palette<-openColours(scheme = "default", n = length(my.breaks))
my.palette <- rev(my.palette)
mapTheme <- rasterTheme(region=my.palette)
# p<-
p<-levelplot(rabnormal*10,
          # main="Landsat NDVI/nNEON Harvard Forest",
          # col.regions=my.col, 
          # col.regions=rev(colfunc(length(my.at))),   
          layout=c(4, 1),
          # col.regions= colorRampPalette(c('#FF0000','white','#000099')),#col.regions=rev(viridis(length(my.at))), 
          par.settings=mapTheme, # add a color ramp
          at=my.breaks, 
          names.attr=pid,
          colorkey=list(space="bottom",labels=list(cex=0.8, font=2, col="black"),
                        height=0.7, width=1),
          xlab=NULL, ylab=NULL, scales=list(draw=FALSE))

outname <- paste0(path_figout,'/peak_snow_storage_maps3_v2_abnormal_dissertation.jpg')
jpeg(outname,width = 12, height=5, units='in',res = 2500)
print(p)
dev.off()


library(StationSWERegressionV2)
# dem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/SNM_dem.tif')
# dem2 <- reprojection_raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/SNM_dem.tif',
#                             'D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif',
#                             'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE',
#                             'bilinear',
#                             'SNM_DEM')
dem <- raster('D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/SNM_DEM.tif')

path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/peak_mean'
fn <- list.files(path, glob2rx('*PeakSWE*.tif'), full.names = T)
fnname <- list.files(path, glob2rx('*PeakSWE*.tif'), full.names = F)
# remove noREc
fn <- fn[-3]
fnname <- fnname[-3]
tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
rstack <- stack(fn)
rstack <- rstack*tem

rstack[[1]] <- rstack[[1]]*100
rstack[[2]] <- rstack[[2]]*100
rstack[[3]] <- rstack[[3]]/10
rstack[[4]] <- rstack[[4]]/10

df <- cbind.data.frame(getValues(dem), getValues(rstack))
names(df) <- c('Elevation', pid)
head(df)
summary(df)
df <- df[complete.cases(df),]
dim(df)

library(plyr)
a <- ddply(df, .(band=cut(Elevation, breaks = seq(1500,max(df$Elevation), by=30))), colwise(mean))
# a$volumne <- a$swe/1000*Area/1000
head(a)
p <- list()
for (i in 1:4) {
  pdf <- cbind.data.frame(Elevation = a$band, SWE = a[,i+1])
  p[[i]]<-
    ggplot(pdf, aes(x=SWE)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    xlim(0,150)+
    ylim(0,0.15)+
    ylab('Density')+
    xlab('Pixel-wise peak SWE (cm)')+
    geom_density(alpha=.2, fill="#FF6666")+
    theme_classic()
}

p2 <- list()
# colors <- c('#7F9F00',"#FC4E07", "#00AFBB","#E7B800",'#8106A9')
colors <- c('#7F9F00',"#FC4E07", "#00AFBB",'#8106A9')
for (i in 1:4) {
  pdf <- cbind.data.frame(Elevation =  seq(1500,max(df$Elevation), by=30), SWE = a[,i+1])
  p2[[i]]<-
    ggplot(pdf,aes(x=Elevation,y=SWE)) + 
    geom_bar(stat = "identity",  position=position_dodge(),color = colors[i], fill='white')+
    # geom_bar(      # Histogram with density instead of count on y-axis
    #                # binwidth=.5,
    #                'identity',
    #                colour="black", fill="white") 
    # xlim(0,150)+
    ggtitle(pid[i]) +
    
    ylim(0,138)+
    xlab('Elevation (m)')+
    ylab('Pixel-wise peak SWE (cm)')+
    # geom_density(alpha=.2, fill="#FF6666")+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, vjust= -10, color='black'),
          axis.title = element_text(size = 13,color='black'),
          axis.text = element_text(size = 12,color = 'black'))
}

library(ggpubr)
ggarrange(p2[[1]],
          p2[[2]]+rremove("ylab"),
          p2[[3]]+rremove("ylab"),
          p2[[4]]+rremove("ylab"),
          # p2[[5]]+rremove("ylab"),
          widths = c(4.2,4,4,4,4),
          nrow = 1,ncol = 4)
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/ele_peakSWE2_mean.jpg'
ggsave(outname, width = 15, height=4, units='in',dpi = 600)

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/0_allinone/ele_peakSWE2_v2.jpg'
ggsave(outname, width = 15, height=4, units='in',dpi = 600)

colors <- c( '#7F9F00',"#FC4E07","#00AFBB",'#8106A9')
lid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
a$Elevation <- seq(1500,max(df$Elevation), by = 30)
pa <- data.frame()
for(i in 1:4){
  pa <- rbind(pa, cbind.data.frame(Elevation = a$Elevation, value = a[, 1+i], id = lid[i]))
}
ggplot(pa)+
  geom_point(aes(x=Elevation, y = value, color = id, shape = id))+
  scale_color_manual(values=colors, labels=lid)+
  scale_shape_manual(values=c(15,16,17,10), labels=lid)+
  theme_classic()+
  ylim(0,138)+
  xlab('Elevation (m)')+
  ylab('Pixel-wise peak SWE (cm)')+
  theme(legend.key=element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_blank(),
        legend.position= c(0.15,0.85),
        legend.background = element_rect(fill = "white"), # get rid of legend bg
        legend.box.background = element_rect(fill = "white"),
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

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/ele_peakSWE2_mean_inone.jpg'
ggsave(outname, width = 8, height=5, units='in',dpi = 600)
