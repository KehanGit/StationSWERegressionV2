
library(raster)
library(rasterVis)
library(ggplot2)
library(ggpubr)



# calculate SWE difference  -----------------------------------------------
runs <- c('20200512_LRM_V1_pl_wfsca_norec')#,'20200509_LRM_V1_pl_wofsca','NWM500','SNODAS500')
ids <- c('norec')#,'wfscadm','wofsca','NWM','SNODAS')
aso <- 'ASO_500'

path_root <- 'D:/GoogleDrive/Research/P2_LRM_test/data/final/LRM_ASO_TU'
path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff'
ftem <- paste0('D:/GoogleDrive/Research/P1_REC_Compare/template/TU/version3/TU500.tif')
tem_tu <- raster(ftem)
if(F){
  
}
for(i in 1:length(runs)){
  fn <- list.files(paste0(path_root,'/', runs[i]), '.tif', full.names = T)
  dir.create(paste0(path_out,'/', ids[i]))
  for(j in 1:length(fn)){
    r <- raster(fn[j])
    if(i>2){
      r <- r/1000
    }
    if(i!=3){
      datestr <- substr(fn[j], nchar(fn[j])-11, nchar(fn[j])-4)
    }else {
      datestr <- substr(fn[j], nchar(fn[j])-15, nchar(fn[j])-8)
    }
    
    faso <- list.files(paste0(path_root,'/', aso), glob2rx(paste0('*', datestr,'*')), full.names = T)
    if(length(faso)==1){
      raso <- raster(faso)
      r[r<0] <- NA
      r <- r*tem_tu
      raso <- raso*tem_tu
      diff <- r - raso
      diff <- diff*100 # unit = cm
      
      outname <- paste0(path_out,'/', ids[i],'/',ids[i],'_', datestr,'.tif')
      writeRaster(diff, outname)
      
    }
  }
}


##plot residuals ------------------
path_plotout <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/comp_figures'
fn <- list.files(paste0(path_out,'/', ids[1],'/'),'.tif', full.names = T)
datestr <- substr(fn, nchar(fn)-11, nchar(fn)-4)
id <-c('LRM-noREC','NWM','SNODAS','LRM-FSCA','LRM-basinline')
for(i in 1:length(fn)){
  subfn <- list.files(paste0(path_out),glob2rx(paste0("*", datestr[i],'*.tif')), full.names = T, recursive = T)
  pid <- paste0(id,'_', datestr[i])
  r_stack <- stack(subfn)
  # Get range of data values across the two rasters
  rng = range(c(getValues(r_stack[[1]]), getValues(r_stack[[2]]),getValues(r_stack[[3]]),
                getValues(r_stack[[4]]),getValues(r_stack[[5]])),na.rm=TRUE)
  
  f<-list()
  for(j in 1:length(subfn)){
    f[[j]]<-
      gplot(r_stack[[j]]) + 
      geom_tile(aes(fill=value)) +
      ggtitle(pid[j]) +
      scale_fill_gradient2(low="red", mid="yellow", high="blue", 
                           midpoint=mean(rng),    # Value that gets the middle color (default is zero)
                           breaks=seq(-100,100,10),  # Set whatever breaks you want
                           limits=c(floor(rng[1]), ceiling(rng[2])))  # Set the same limits for each plot
  }
  
  ggarrange(f[[1]],
            f[[2]],
            f[[3]],
            f[[4]],
            f[[5]],
            heights = c(5,5),
            widths = c(5,5,5),
            nrow = 2,ncol = 3)
  outname <-  paste0(path_plotout,'/ASO_diff_comp_',datestr[i],'.jpg')
  # ggsave(outname, width = 15, height=8, units='in',dpi = 600)
}


# residual regression -----
fn <- list.files(paste0(path_root,'/20200509_LRM_V1_pl_wofsca'), '.tif', full.names = T)
length(fn)
pr <- data.frame()
c <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date.txt')
fn <- fn[substr(fn,nchar(fn)-11, nchar(fn)-4) %in% c[,1]]
for (i in 1:length(c[,1])) {
  r <- raster(fn[i])
  r[r<0] <- 0
  faso <- list.files(paste0(path_root,'/', aso), glob2rx(paste0('*', c[i,1],'*')), full.names = T)
  if(length(faso)==1){
    rv <- getValues(r)
    raso <- getValues(raster(faso))
    rdf <- cbind.data.frame(obs=raso, sim=rv)
  }
  rdf <- rdf[complete.cases(rdf),]
  pr <- rbind(pr,rdf)
}

pr <- pr[complete.cases(pr),]
res_bl <- lm(pr$obs ~ pr$sim)
summary(res_bl)

res_dm <- lm(pr$obs ~ pr$sim)
summary(res_dm)


# residual correlation with elevation, aspect, slope and tree cover ---------------

# create the variable tempolates for the Sierra and TU
if(F){
  path <- 'D:/Data/VegCover'
  data <- list.files(path,glob2rx('*treecover2010*.tif'), full.names = T)
  library(raster)
  r1 <- raster(data[1])
  r2 <- raster(data[2])
  r3 <- raster(data[3])
  r4 <- raster(data[4])
  
  r5 <- merge(r1,r2)
  r6 <- merge(r3,r4)
  r7 <- merge(r5,r6)
  writeRaster(r7,'D:/Data/VegCover/treecover2010_mosiac.tif')
  data <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/SNM_500_DEM.tif')
  library(StationSWERegressionV2)
  reprojection_raster(fimage = 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/Data/temp/Sierra/SNM_aspect.tif',
                      ftemp = 'D:/GoogleDrive/Research/P1_REC_Compare/template/TU/version3/TU500.tif',
                      prefix = 'TU_aspect',
                      dir = 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/Data/temp/TU',
                      method = 'bilinear')
  
  fn <- list.files('D:/GoogleDrive/Research/P2_LRM_test/input/res500/phv','.tif', full.names = T)
  fnname <- list.files('D:/GoogleDrive/Research/P2_LRM_test/input/res500/phv','.tif', full.names = F)
  varnames <- sapply(strsplit(fnname,'_'),'[',2)
  varnames <- paste0("TU_",varnames)
  varnames <- sapply(strsplit(varnames,".tif"),'[',1)
  for(i in 1:length(fn)){
    reprojection_raster(fimage = fn[i],
                        ftemp = 'D:/GoogleDrive/Research/P1_REC_Compare/template/TU/version3/TU500.tif',
                        prefix = varnames[i],
                        dir = 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/Data/temp/TU_all',
                        method = 'bilinear')
  }
  
}

# calculate r2 for each day 
path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff'
datelist <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date.txt')
pid <- c("LRM_noREC",'NWM','SNODAS','LRM-FSCA','LRM-baseline')
dir = 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/Data/temp/TU_all'
fn_var <- list.files(dir,'.tif', full.names = T)
r_var <- stack(fn_var)
names_var <- c('Aspect','Elevation','Slope','Tree-Cover')
df_var <- as.data.frame(r_var)
wdata <- data.frame()
for(i in 1:length(datelist[,1])){
  datestr <- datelist[i,1]
  fn <- list.files(path_out,glob2rx(paste0("*", datestr, "*.tif")), full.names = T, recursive = T)
  if(length(fn)==5){
    for(j in 1:length(fn)){
      r <- raster(fn[j])
      df <- cbind.data.frame(error = getValues(r), df_var)
      df <- df[complete.cases(df),]
      for(k in 2:ncol(df)){
        pvalue <- summary(lm(df$error ~ df[,k]))$coefficient[2,4]
        r2 <- summary(lm(df$error ~ df[,k]))$r.squared
        wdata <- rbind(wdata,cbind.data.frame(date=datestr, pvalue, r2, dateset=pid[j],var = names(r_var)[k-1]))
      }
    }
  }else{
    print(paste0('Something is wrong on the date of ', datestr))
  }
  
}
dim(wdata)
dim(wdata[wdata$pvalue > 0.01,])
wdata_sig <- wdata[wdata$pvalue < 0.01,]
avg_wdata_sig <- 
  wdata_sig %>%
  group_by(var, dateset) %>%
  summarise(r2 = mean(r2))


ggplot(data=avg_wdata_sig,aes(x=dateset,y=r2,fill=var))+
  geom_bar(stat="identity",width=1,colour="black",size=0.1,position="dodge")+
  coord_polar()+
  scale_fill_brewer(palette="Greens")+
  xlab("")+ylab("")


colors <- c('#7F9F00',"#FC4E07", "#00AFBB","#E7B800",'#8106A9')
ggplot(data=avg_wdata_sig,aes(x=var,y=r2,fill=dateset))+
  geom_bar(stat="identity",position="dodge", alpha=0.8)+
  coord_polar()+
  scale_fill_manual(values = colors)+
  xlab("")+ylab("")+
  theme_bw()
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/error_rose_plot_600dpi.jpg'
ggsave(outname, width = 10, height=8, units='in',dpi = 600)



ggplot(data=avg_wdata_sig,aes(x=var,y=r2,fill=dateset))+
  geom_bar(stat="identity",position="dodge",colour="black")+
  # coord_polar()+
  # scale_fill_brewer(palette="Greens")+
  xlab("")+ylab("")+
  theme_bw()




# calculate r2 for each day -----
path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff'
datelist <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date.txt')
pid <- c("LRM_noREC",'NWM','SNODAS','LRM-FSCA','LRM-baseline')
dir = 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/Data/temp/TU'
fn_var <- list.files(dir,'.tif', full.names = T)
r_var <- stack(fn_var)
names_var <- c('Aspect','Elevation','Slope','Tree Cover')
df_var <- as.data.frame(r_var)
wdata <- data.frame()
for(i in 1:length(datelist[,1])){
  datestr <- datelist[i,1]
  fn <- list.files(path_out,glob2rx(paste0("*", datestr, "*.tif")), full.names = T, recursive = T)
  if(length(fn)==5){
    for(j in 1:length(fn)){
      r <- raster(fn[j])
      df <- cbind.data.frame(error = getValues(r), df_var)
      df <- df[complete.cases(df),]
      for(k in 2:ncol(df)){
        pvalue <- summary(lm(df$error ~ df[,k]))$coefficient[2,4]
        r2 <- summary(lm(df$error ~ df[,k]))$r.squared
        wdata <- rbind(wdata,cbind.data.frame(date=datestr, pvalue, r2, dateset=pid[j],var = names(r_var)[k-1]))
      }
    }
  }else{
    print(paste0('Something is wrong on the date of ', datestr))
  }
  
}
dim(wdata)
dim(wdata[wdata$pvalue > 0.05,])
# wdata_sig <- wdata[wdata$pvalue < 0.05,]
# write.csv(wdata,'cor_data.csv')title(datelist[k])+
setwd('D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis')
wdata <- read.csv('cor_data.csv')
wdata_sig <- wdata[wdata$pvalue<0.05,]
datelist <- unique(wdata_sig$date)
names_var <- c('Aspect','Elevation','Slope','Tree Cover')
wdata_sig$varname <- NA
wdata_sig$varname[wdata_sig$var == 'TU_slope'] <- 'Slope'
wdata_sig$varname[wdata_sig$var == 'TU_TC'] <- 'Tree Cover'
wdata_sig$varname[wdata_sig$var == 'TU_DEM'] <- 'Elevation'
wdata_sig$varname[wdata_sig$var == 'TU_aspect'] <- 'Aspect'

wdata_sig$order[wdata_sig$dateset == 'LRM-FSCA'] <- 1
wdata_sig$order[wdata_sig$dateset == 'LRM-baseline'] <- 2
wdata_sig$order[wdata_sig$dateset == 'LRM_noREC'] <- 3
wdata_sig$order[wdata_sig$dateset == 'SNODAS'] <- 4
wdata_sig$order[wdata_sig$dateset == 'NWM'] <- 5

colors <- c('#7F9F00',"#FC4E07", "#00AFBB","#E7B800",'#8106A9')

for(i in 1:length(datelist)){
  pdata_sig <- wdata_sig[wdata_sig$date == datelist[i],]
  f <- 
    pdata_sig %>%
    mutate(dateset = fct_reorder(dateset, order)) %>%
    ggplot()+
    geom_bar(aes(x=varname,y=r2,fill=dateset),stat="identity",position="dodge", alpha=0.8,show.legend = F)+
    coord_polar()+
    # title(datelist[i])+
    ylim(c(0,0.45))+
    scale_fill_manual(values = colors)+
    xlab("Topographic Feature")+ylab("R-squared")+
    theme_bw()+
    theme(#plot.title = element_text(size = 15, datelist[i]),
          axis.text = element_text(size = 15, color = 'black'),
          axis.title = element_text(size = 15, color = 'black'))
          #panel.background = element_blank(),)
    # 
  print(f)
  outname <- paste0('daily/',datelist[i],'_ASO_rose_error.jpg')
  ggsave(outname, width = 4, height=4, units='in',dpi = 600)
  # i=i+1
}


avg_wdata_sig <- 
  wdata_sig %>%
  group_by(var, dateset) %>%
  summarise(r2 = mean(r2))

colors <- c('#7F9F00',"#FC4E07", "#00AFBB","#E7B800",'#8106A9')
ggplot(data=avg_wdata_sig,aes(x=var,y=r2,fill=dateset))+
  geom_bar(stat="identity",position="dodge", alpha=0.8)+
  coord_polar()+
  scale_fill_manual(values = colors)+
  xlab("")+ylab("")+
  theme_bw()
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/error_4vars_rose_plot_600dpi.jpg'
ggsave(outname, width = 10, height=8, units='in',dpi = 600)



# calculate r2 overall -----
if(F){
  path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/data/images/ASO_Diff'
  datelist <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date.txt')
  pid <- c("LRM_noREC",'NWM','SNODAS','LRM-FSCA','LRM-baseline')
  dir = 'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/Data/temp/TU'
  fn_var <- list.files(dir,'.tif', full.names = T)
  r_var <- stack(fn_var)
  names_var <- c('Aspect','Elevation','Slope','Tree-Cover')
  df_var <- as.data.frame(r_var)
  df_all <- data.frame()
  for(i in 1:length(datelist[,1])){
    datestr <- datelist[i,1]
    fn <- list.files(path_out,glob2rx(paste0("*", datestr, "*.tif")), full.names = T, recursive = T)
    if(length(fn)==5){
      for(j in 1:length(fn)){
        r <- raster(fn[j])
        df <- cbind.data.frame(error = getValues(r), df_var, dateset=pid[j])
        df <- df[complete.cases(df),]
        df_all <- rbind(df_all, df)
      }
    }else{
      print(paste0('Something is wrong on the date of ', datestr))
    }
    
  }
  
  wdata <- data.frame()
  for(j in 1:length(fn)){
    for(k in 2:(ncol(df_all)-1)){
      sub_df <- df_all[df_all$dateset == pid[j],]
      pvalue <- summary(lm(sub_df$error ~ sub_df[,k]))$coefficient[2,4]
      r2 <- summary(lm(sub_df$error ~ sub_df[,k]))$r.squared
      wdata <- rbind(wdata,cbind.data.frame(date=datestr, pvalue, r2, dateset=pid[j],var = names(r_var)[k-1]))
    }
  }
  
  
  dim(wdata)
  dim(wdata[wdata$pvalue > 0.01,])
  wdata_sig <- wdata[wdata$pvalue < 0.01,]
  avg_wdata_sig <- 
    wdata_sig %>%
    group_by(var, dateset) %>%
    summarise(r2 = mean(r2))
  
  colors <- c('#7F9F00',"#FC4E07", "#00AFBB","#E7B800",'#8106A9')
  ggplot(data=avg_wdata_sig,aes(x=var,y=r2,fill=dateset))+
    geom_bar(stat="identity",position="dodge", alpha=0.8)+
    coord_polar()+
    scale_fill_manual(values = colors)+
    xlab("")+ylab("")+
    theme_bw()
  outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/error_4vars_overall_rose_plot_600dpi.jpg'
  ggsave(outname, width = 10, height=8, units='in',dpi = 600)
  
  avg_wdata_sig2 <- avg_wdata_sig[avg_wdata_sig$dateset %in% c('LRM-baseline'),]
  ggplot(data=avg_wdata_sig2,aes(x=var,y=r2,fill=dateset))+
    geom_bar(stat="identity",position="dodge", alpha=0.8)+
    coord_polar()+
    scale_fill_manual(values = colors)+
    xlab("")+ylab("")+
    theme_bw()
  outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/5_error_analysis/error_4vars_overall_rose_plot_600dpi_forinterview.jpg'
  ggsave(outname, width = 10, height=8, units='in',dpi = 600)
  
  avg_wdata_sig2 <- avg_wdata_sig[avg_wdata_sig$dateset %in% c('NWM'),]
  ggplot(data=avg_wdata_sig2,aes(x=var,y=r2,fill=dateset))+
    geom_bar(stat="identity",position="dodge", alpha=0.8)+
    coord_polar()+
    scale_fill_manual(values = colors)+
    xlab("")+ylab("")+
    theme_bw()
  
  
}

