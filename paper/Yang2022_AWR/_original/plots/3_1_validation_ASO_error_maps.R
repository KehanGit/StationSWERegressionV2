#plot the SWE residuals in the TU for Five SWE datasets

#### create water mask for TU and Sierra #############
if(F){
  water <- raster('D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask/SNW_waterboday_0120new.tif')
  a <- reprojection_raster(fimage = 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask/SNW_waterboday_0120new.tif',
                           ftemp = 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff/norec/norec_20160326.tif',
                           dir = 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask',method = 'near',
                           prefix = 'water_mask_TU')
  
  b <- reprojection_raster(fimage = 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask/SNW_waterboday_0120new.tif',
                           ftemp = 'D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif',
                           dir = 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/watermask',method = 'near',
                           prefix = 'water_mask_Sierra')
  b1 <- raster(b)
  b1[b1==1] <- NA
  plot(b1)
  c = raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
  c[is.na(b1)]<-NA
  plot(c)
  writeRaster(c,'D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500_WM.tif')
  d <- raster( 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff/norec/norec_20160326.tif')
  d[!is.na(d)] <- 1
  plot(d)
  e = raster(a)
  plot(e)
  e[e==1] <- NA
  d[is.na(e)] <- NA
  writeRaster(d,'D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/TU_500_WM.tif')
}
###################### end ########################

library(rasterVis)
library(ggpubr)
library(colorRamps)
library(wesanderson) #https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
library(raster)
library(rgdal)
pid <-c('LRM-basinline','LRM-noREC','LRM-FSCA','SNODAS','NWM')
order <- c(5,1,4,3,2)
datelist <- c('20130403','20140407','20150217','20160326','20170401')
path_data <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff'
path_figout <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/diff_maps'
#unit = cm
maxswe <- 100
minswe <- -100
cols <- colorRampPalette(brewer.pal(9,"YlGn"))
nlev <- 100
my.at <- seq(minswe,maxswe,
             length.out = nlev + 1)
my.theme <- rasterTheme(viridis_pal(option = "D")(nlev))
my.theme <- rasterTheme(Zissou1(nlev))
my.col <- rev(wes_palette("Zissou1", nlev, type = "continuous"))
mapTheme <- rasterTheme(region=brewer.pal(8,"BrBG"))
my.col <- rainbow(5)
p <- list()
TUB_shp <- readOGR('D:/Data/watersheds/BasinBoundaryShp/TuolumneBasin_ASO_UTM11.shp', layer = 'TuolumneBasin_ASO_UTM11')
temp = raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/TU_500_WM.tif')


for(i in 1:length(fn)){
  fn <- list.files(path_data, glob2rx(paste0('*',datelist[i],'*.tif')), full.names = T, recursive = T)  
  fn <- fn[order]
  
  r_stack <- stack(fn)
  r_stack_ori <- r_stack
  r_stack_ori <- r_stack_ori * temp
  pmax <- max(getValues(r_stack), na.rm = T)
  pmin <- min(getValues(r_stack), na.rm = T)
  print(pmax)
  print(pmin)
  
  
  p_higher <- max(abs(pmax), abs(pmin))
  pmin <- (-1) * p_higher
  pmax <- (1) * p_higher
  
  
  # normalization
  # 
  # r_stack[r_stack>maxswe] <- 150
  # r_stack[r_stack<minswe] <- -150
  r_stack <- r_stack/p_higher
  
  # minswe <- min(getValues(r_stack), na.rm=T)
  # maxswe <- max(getValues(r_stack), na.rm=T)
  
  # maxscale <- max(abs(minswe),abs(maxswe))
  # maxswe <- maxscale
  # minswe <- -1 * maxscale
  # 
  my.at <- seq(-1,1,
               length.out = nlev + 1)
  
  p[[i]] <-
    levelplot(r_stack,
              layout=c(1, 5),
              main= datelist[i],
              col.regions= colorRampPalette(c('#FF0000','white','#000099')),#rev(matlab.like(nlev + 1)),
              at = my.at,
              # par.setting = mapTheme,
              par.settings = list(axis.line = list(col = "transparent"),
                                  strip.background = list(col = 'transparent'), 
                                  strip.border = list(col = 'transparent')),
              names.attr=rep('',5),
              plot.margin=unit(c(1,-1,1,-1), "cm"),
              scales=list(draw=F ),# remove axes labels & ticks
              colorkey=F)+layer(sp.polygons(TUB_shp, col = 'gray40'))
  
  
  
}
p[[6]] <- levelplot(r_stack,
                    layout=c(1, 5),
                    main= datelist[i],
                    col.regions=colorRampPalette(c('dark red','white','blue')),#rev(matlab.like(nlev + 1)),
                    at = my.at,
                    # par.setting = mapTheme,
                    par.settings = list(axis.line = list(col = "transparent"),
                                        strip.background = list(col = 'transparent'), 
                                        strip.border = list(col = 'transparent')),
                    names.attr=rep('',5),
                    plot.margin=unit(c(1,-1,1,-1), "cm"),
                    # par.settings = my.padding,
                    scales=list(draw=F),# remove axes labels & ticks
                    colorkey=list(space='right'))




ggarrange(p[[1]],
          p[[2]],
          p[[3]],
          p[[4]],
          p[[5]],
          widths = c(4,4,4,4,4),
          nrow = 1,ncol = 5)
path_figout <- 'D:/GoogleDrive/Research/P2_LRM_test/0submittion/figures'
outname <- paste0(path_figout,'/5wy_diffmap_new_0128.tiff')
ggsave(outname, width = 17, height=17, units='in',dpi = 600)

ggarrange(p[[6]],
          widths = c(5),
          nrow = 1,ncol = 1)
outname <- paste0(path_figout,'/5wy_diffmap_new_legend_0128.tiff')
ggsave(outname, width = 5, height=17, units='in',dpi = 600)

outname <-  paste0(path_figout,'/5wy_diffmap_legend_0128.jpg')
f <- levelplot(r_stack,
               layout=c(1, 5),
               main= paste0("SWE error on ", datelist[i]),
               col.regions=my.col,
               at = my.at,
               # par.setting = mapTheme,
               par.settings = list(axis.line = list(col = "transparent"),
                                   strip.background = list(col = 'transparent'), 
                                   strip.border = list(col = 'transparent')),
               names.attr=pid,
               scales=list(draw=FALSE ),# remove axes labels & ticks
               colorkey=list(space='bottom'))
jpeg(outname,width = 10, height=16, units='in',res = 600)
print(f)
dev.off()


