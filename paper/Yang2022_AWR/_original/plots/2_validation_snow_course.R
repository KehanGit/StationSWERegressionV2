
# validate five SWE products with ground observations, 9 pixel, mininum difference 
# kehan Yang 20200407

# modify 20210816 KH
## switch x and y axis. x should be observation, y should be model simulation. 
## This is different from dependent/independent variable


# 0 load libraries --------------------------------------------------------

library(lubridate)
library(StationSWERegressionV2)
library(ggpubr)
library(dplyr)
library(RColorBrewer)

#step 1: read station data -------------------------------------------------------
fn <- list.files('D:/GoogleDrive/Research/P1_REC_Compare/validationbysnowcourse/snowcourse_ori','.txt', full.name=T)
sc_data <- NA
for(i in 1:length(fn)){
  data <- read.csv(fn[i])
  sc_data <- rbind(sc_data, data)
}
sc_data <- sc_data[-1,]

# relate fid to station ID
sc_info <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/data/studyarea/snowcourse_info.txt')
names(sc_info)[2] <- c('Site_ID')
sc_data <- merge(sc_data, sc_info[,c('FID','Site_ID')], by.x = 'ID', by.y = 'Site_ID')
names(sc_data)[1] <- c('Site_ID')
head(sc_data)
orisc_data <- sc_data


#step 2 sc validation plot-------------------------------------------------
if(T){
  
  dfalpha <- 0.1
  dfsize <- 250
  axis_x_size <- 13
  axis_y_size <- 13
  axis_labx_size <- 12
  axis_laby_size <- 12
  legend_size <- 4.2
  dflinecolor <- 'red'
  
  path_data <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/2_validation_snow_course/data'
  
  # manuscript version ---
  # fn_sc <- list.files(path_data, glob2rx(paste0('*sc.txt')), full.names = T)
  # fnname_sc <- list.files(path_data, glob2rx(paste0('*sc.txt')), full.names = F)
  # fn_sc <- fn_sc[-3]
  # fnname_sc <- fnname_sc[-3]
  
  # review vesion --- 20210816
  fn_sc <- list.files(path_data, glob2rx(paste0('*sc_review.txt')), full.names = T)
  fnname_sc <- list.files(path_data, glob2rx(paste0('*sc_review.txt')), full.names = F)
  
  
  id <- sapply(strsplit(fnname_sc, '_sc'),'[',1)
  # pid<-c('LRM-FSCA','LRM-baseline','LRM-noREC','SNODAS','NWM-SWE')
  # colors <- c( '#7F9F00',"#FC4E07","#00AFBB","#E7B800",'#8106A9') 
  pid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
  colors <- c( '#7F9F00',"#FC4E07","#00AFBB",'#8106A9')
  
  # remove norec
  
  #-- need to change the number of column of SWE data
  all <- read.csv(fn_sc[1], stringsAsFactors = F)
  # swe_wfsca selected modeled SWE when compared with fsca_scaled snow course obs
  # swe_wofsca selected modeled SWE when compared with snow course obs
  # sc_fscascaled = fsca * snow course obs
  
  # all <- all[,-5]
  names(all)[6] <- paste0(id[1],'_sc_scaled')
  names(all)[7] <- paste0(id[1],'_fsca')
  names(all)[8] <- paste0(id[1],'_ori')
  names(all)[9] <- paste0(id[1],'_ori_wofsca')
  head(all)
  
  for(i in 2:length(fn_sc)){
    data <- read.csv(fn_sc[i], stringsAsFactors = F)
    # names(data)[6] <- paste0(id[i],'_scaled')
    names(data)[6] <- paste0(id[i],'_sc_scaled')
    names(data)[7] <- paste0(id[i],'_fsca')
    names(data)[8] <- paste0(id[i],'_ori')
    names(data)[9] <- paste0(id[i],'_ori_wofsca')
    # all <- merge(all, data[,c('Site_ID','DateObs',paste0(id[i],'_scaled'), paste0(id[i],'_fsca'), paste0(id[i],'_ori'))], by = c('Site_ID','DateObs'))
    all <- merge(all, data[,c('Site_ID','DateObs',paste0(id[i],'_sc_scaled'), paste0(id[i],'_fsca'),
                              paste0(id[i],'_ori'),paste0(id[i],'_ori_wofsca'))],
                 by = c('Site_ID','DateObs'))
  }
  
  dim(all)
  sc_buff <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/data/studyarea/snowcourse_info_2kmbuff.txt')
  all <- all[all$Site_ID %in% sc_buff$STA,]
  dim(all)
  i=1
  # all <- all[all$SWE > 0,]
  # for(i in 1:length(fn_sc)){
  #   #remove outlines > 4000
  #   all <- all[which(all[,paste0(id[i],'_scaled')] < 4000),]
  # }
  dim(all)
  # remove the date that with poor LRM SWE estimates 
  # date_qc <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/2_validation_snow_course/qc_th_model_40_v2.txt')
  date_qc <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/2_validation_snow_course/LRM_date_mdGT40.txt')
  
  all <- all[all$DateObs %in% date_qc$Date,]
  dim(all)
  # manuscript 1 has 6407 pairs 
  # review has 6592 pairs 
  labels <- c('A','B','C','D')
  
  # incloude elevation info
  all <- merge(all, sc_info[,c('Site_ID','Elevation')], by = 'Site_ID')
  all$Elevation_m <- all$Elevation * 0.3048 # feet to meter
  all$DOY <- as.numeric(strftime(as.Date(as.character(all$DateRep),'%Y%m%d'), '%j'))
  
  if(T){
    p1 <- list()
    p2 <- list()
    p3 <- list()
    rlist <- NA
    rmselist <- NA
    rmaelist <- NA
    pbiaslist <- NA
    for(i in 1:length(fn_sc)){
      
      # df <- cbind.data.frame(DateObs = all$DateObs,
      #                        DateRep = all$DateRep,
      #                        SWE=all$SWE*all[,paste0(id[i],'_fsca')]/100, swe_scaled=all[,paste0(id[i],'_ori')], 
      #                        swe_ori = all[,paste0(id[i],'_ori'),],
      #                        fsca = all[,paste0(id[i],'_fsca')],
      #                        month = substr(all$DateRep,5,6))
      # here SWE is fsca * snow course SWE; 
      # swe_scaled is the original modeled SWE with minimum difference compared with snow course 
      
      df <- cbind.data.frame(DateObs = all$DateObs,
                             DateRep = all$DateRep,
                             SWE=all[,paste0(id[i],'_sc_scaled')], swe_scaled=all[,paste0(id[i],'_ori')], 
                             # swe_ori = all[,paste0(id[i],'_ori'),],
                             fsca = all[,paste0(id[i],'_fsca')],
                             month = substr(all$DateRep,5,6),
                             DOY = all$DOY,
                             Elevation = all$Elevation_m)
      
      # here SWE is fsca * snow course SWE; 
      # swe_scaled is the original modeled SWE with minimum difference compared with fsca-scaled snow course 
      
      
      r <- cor.test(df$SWE, df$swe_scaled)
      r <- as.numeric(r$estimate)
      r <- r * r
      rlist <- rbind(rlist, r)
      r <- format(round(r, 2), nsmall = 2)
      
      
      rmae <- hydroGOF::mae(df$SWE, df$swe_scaled)/mean(all$SWE)*100
      rmaelist <- rbind(rmaelist, rmae)
      rmae <-  format(round(rmae, 1), nsmall = 1)
      rmae <- paste0(rmae, ' %')
      
      rmse <- hydroGOF::rmse(df$SWE, df$swe_scaled)/10
      rmselist <- rbind(rmselist, rmse)
      rmse <-  format(round(rmse, 1), nsmall = 1)
      rmse <- paste0(rmse, ' cm')
      
      slope <- lm(df$SWE ~ df$swe_scaled)
      slope <- as.numeric(slope$coefficients[2])
      slope <-  format(round(slope, 2), nsmall = 2)
      
      pbias <- (sum(df$swe_scaled - df$SWE))/(sum(df$SWE)) * 100
      pbiaslist <- rbind(pbiaslist, pbias)
      pbias <- format(round(pbias, 1), nsmall = 1)
      pbias <- paste0(pbias,' %')
      
      xloc <- 120
      yloc <- 50
      yint <- 15
      
      df$SWEcm <- df$SWE/10
      df$swe_scaledcm <- df$swe_scaled/10
      
      p1[[i]] <-
        df %>%
        ggplot(aes(x= SWEcm, y = swe_scaledcm))+
        geom_point(color = colors[i], alpha=0.25)+
        geom_abline(slope = 1, color = 'black', size=1)+
        # geom_density_2d(aes(colour  = (..level..)))+
        # geom_density_2d(aes(colour = cut))+
        # geom_density_2d_filled(contour_var = "ndensity")+
        # stat_density_2d(geom = "polygon", contour = TRUE,
        #                 aes(fill = (..level..)), colour = "black",
        #                 bins = 5)+
        annotate('text', x = 20, y= 180, label = labels[i], size=8, colour = "black", parse = TRUE)+
        # smoothScatter()+
        geom_smooth(method='lm', color = dflinecolor, size = 1)+
        # 0 means left-justified, 0.5 means centered, and 1 means right-justified.
        annotate('text', x =xloc, y = yloc, label = paste("R^2: ",as.character(r)) ,hjust = 0, size = legend_size, parse=TRUE, color = 'black')+
        annotate('text', x =xloc, y = yloc-yint, label = paste('RMSE:', as.character(rmse)),hjust = 0,size = legend_size, color = 'black')+
        annotate('text', x =xloc, y = yloc-2*yint, label = paste('RMAE:', as.character(rmae)),hjust = 0,size = legend_size, color = 'black')+
        annotate('text', x =xloc, y = yloc-3*yint, label = paste('PBIAS:', as.character(pbias)),hjust = 0,size = legend_size, color = 'black')+
        # annotate('text', x =250, y = 1640, label = paste('UE:', as.character(underestimation),'%'))+
        ylim(c(0,200))+
        xlim(c(0,200))+
        ylab(paste(pid[i],'(cm) '))+
        xlab('Snow course SWE (cm)')+
        theme(#legend.text = element_text(size=legend_size),
          # legend.title = element_text(size = legend_size+2),
          axis.title.x = element_text(size=axis_x_size, color = 'black'),
          axis.title.y = element_text(size=axis_y_size, color = 'black'),
          axis.text.x = element_text(size = axis_labx_size, hjust = 0.4, color = 'black'),
          axis.text.y = element_text(size = axis_laby_size, color = 'black'),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black")
        )
      
      
      # p2[[i]] <-
      #   df %>%
      #   ggplot()+
      #   geom_point(aes(x= SWEcm, y = swe_scaledcm, color = month), alpha=1)+
      #   geom_abline(slope = 1, color = 'black', size=1)+
      #   geom_density_2d(aes(y= SWEcm, x = swe_scaledcm),colour='gray50')+
      #   # facet_wrap(~ day, ncol=1)
      #   # smoothScatter()+
      #   # geom_smooth(method='lm', color = dflinecolor, size = 1)+
      #   # 0 means left-justified, 0.5 means centered, and 1 means right-justified.
      #   annotate('text', x =xloc, y = yloc, label = paste("R^2: ",as.character(r)) ,hjust = 0, size = legend_size, parse=TRUE, color = 'black')+
      #   annotate('text', x =xloc, y = yloc-yint, label = paste('RMAE:', as.character(rmae)),hjust = 0,size = legend_size, color = 'black')+
      #   annotate('text', x =xloc, y = yloc-2*yint, label = paste('RMSE:', as.character(rmse)),hjust = 0,size = legend_size, color = 'black')+
      #   annotate('text', x =xloc, y = yloc-3*yint, label = paste('PBIAS:', as.character(pbias)),hjust = 0,size = legend_size, color = 'black')+
      #   # annotate('text', x =250, y = 1640, label = paste('UE:', as.character(underestimation),'%'))+
      #   ylim(c(0,200))+
      #   xlim(c(0,200))+
      #   ylab(paste(pid[i],'(cm) '))+
      #   xlab('Snow course SWE (cm)')+
      #   theme(#legend.text = element_text(size=legend_size),
      #     # legend.title = element_text(size = legend_size+2),
      #     axis.title.x = element_text(size=axis_x_size, color = 'black'),
      #     axis.title.y = element_text(size=axis_y_size, color = 'black'),
      #     axis.text.x = element_text(size = axis_labx_size, hjust = 0.4, color = 'black'),
      #     axis.text.y = element_text(size = axis_laby_size, color = 'black'),
      #     panel.background = element_blank(),
      #     panel.border = element_blank(),
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank(),
      #     # Change axis line
      #     axis.line = element_line(colour = "black")
      #   )
      # 
      myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
      p2[[i]]<-
        df %>%
        ggplot()+
        geom_point(aes(x= SWEcm, y = swe_scaledcm, color = as.factor(month)), alpha=0.8, show.legend = F)+
        geom_abline(slope = 1, color = 'black', size=1)+
        # geom_density_2d(aes(y= SWEcm, x = swe_scaledcm),colour='gray50')+
        geom_smooth(aes(x= SWEcm, y = swe_scaledcm),method='lm', color = dflinecolor, size = 1)+
        # facet_wrap(~ day, ncol=1)
        # smoothScatter()+
        # geom_smooth(method='lm', color = dflinecolor, size = 1)+
        # 0 means left-justified, 0.5 means centered, and 1 means right-justified.
        annotate('text', x =xloc, y = yloc, label = paste("R^2: ",as.character(r)) ,hjust = 0, size = legend_size, parse=TRUE, color = 'black')+
        annotate('text', x =xloc, y = yloc-yint, label = paste('RMAE:', as.character(rmae)),hjust = 0,size = legend_size, color = 'black')+
        annotate('text', x =xloc, y = yloc-2*yint, label = paste('RMSE:', as.character(rmse)),hjust = 0,size = legend_size, color = 'black')+
        annotate('text', x =xloc, y = yloc-3*yint, label = paste('PBIAS:', as.character(pbias)),hjust = 0,size = legend_size, color = 'black')+
        # annotate('text', x =250, y = 1640, label = paste('UE:', as.character(underestimation),'%'))+
        ylim(c(0,200))+
        xlim(c(0,200))+
        ylab(paste(pid[i],'(cm) '))+
        xlab('Snow course SWE (cm)')+
        scale_color_brewer(palette="Spectral")+
        # scale_color_brewer(colours = colorRampPalette(rev(brewer.pal(6, "Spectral"))))+
        theme(#legend.text = element_text(size=legend_size),
          # legend.title = element_text(size = legend_size+2),
          axis.title.x = element_text(size=axis_x_size, color = 'black'),
          axis.title.y = element_text(size=axis_y_size, color = 'black'),
          axis.text.x = element_text(size = axis_labx_size, hjust = 0.4, color = 'black'),
          axis.text.y = element_text(size = axis_laby_size, color = 'black'),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black")
        )
      
      
      
      p3[[i]]<-
        df %>%
        ggplot()+
        geom_point(aes(x= SWEcm, y = swe_scaledcm, color = Elevation), alpha=0.5, show.legend = F)+
        geom_abline(slope = 1, color = 'black', size=1)+
        # geom_density_2d(aes(y= SWEcm, x = swe_scaledcm),colour='gray50')+
        geom_smooth(aes(x= SWEcm, y = swe_scaledcm),method='lm', color = dflinecolor, size = 1)+
        # facet_wrap(~ day, ncol=1)
        # smoothScatter()+
        # geom_smooth(method='lm', color = dflinecolor, size = 1)+
        # 0 means left-justified, 0.5 means centered, and 1 means right-justified.
        annotate('text', x =xloc, y = yloc, label = paste("R^2: ",as.character(r)) ,hjust = 0, size = legend_size, parse=TRUE, color = 'black')+
        annotate('text', x =xloc, y = yloc-yint, label = paste('RMAE:', as.character(rmae)),hjust = 0,size = legend_size, color = 'black')+
        annotate('text', x =xloc, y = yloc-2*yint, label = paste('RMSE:', as.character(rmse)),hjust = 0,size = legend_size, color = 'black')+
        annotate('text', x =xloc, y = yloc-3*yint, label = paste('PBIAS:', as.character(pbias)),hjust = 0,size = legend_size, color = 'black')+
        # annotate('text', x =250, y = 1640, label = paste('UE:', as.character(underestimation),'%'))+
        ylim(c(0,200))+
        xlim(c(0,200))+
        ylab(paste(pid[i],'(cm) '))+
        xlab('Snow course SWE (cm)')+
        # scale_color_gradient(low="blue", high="red")+
        scale_colour_gradientn(colours = myPalette(100), limits=c(min(df$Elevation), max(df$Elevation)))+
        theme(#legend.text = element_text(size=legend_size),
          # legend.title = element_text(size = legend_size+2),
          axis.title.x = element_text(size=axis_x_size, color = 'black'),
          axis.title.y = element_text(size=axis_y_size, color = 'black'),
          axis.text.x = element_text(size = axis_labx_size, hjust = 0.4, color = 'black'),
          axis.text.y = element_text(size = axis_laby_size, color = 'black'),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Change axis line
          axis.line = element_line(colour = "black")
        )
      
      
      
    }
    
    ggarrange(p1[[1]],
              p1[[2]],
              p1[[3]],#+rremove("ylab"),
              p1[[4]],
              # p1[[5]]+rremove("ylab"),
              widths = c(4.2,4.2),
              nrow = 2,ncol = 2)
    
    # ggarrange(p2[[1]],
    #           p2[[2]]+rremove("ylab"),
    #           p2[[3]]+rremove("ylab"),
    #           p2[[4]]+rremove("ylab"),
    #           # p1[[5]]+rremove("ylab"),
    #           widths = c(4.2,4,4,4),
    #           nrow = 1,ncol = 4)
  }
}
path_fig <- 'D:/GoogleDrive/Research/P2_LRM_test/0submittion/figures'
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_review.jpg')
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_scaled_snowcourse_vs_model.jpg')
ggsave(outname, width = 8, height=8, units='in',dpi = 600)

ggarrange(p2[[1]],
          p2[[2]],
          p2[[3]],#+rremove("ylab"),
          p2[[4]],
          # p1[[5]]+rremove("ylab"),
          widths = c(4.2,4.2),
          nrow = 2,ncol = 2)
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_scaled_snowcourse_vs_model_monthly.jpg')
ggsave(outname, width = 8, height=8, units='in',dpi = 600)

ggarrange(p3[[1]],
          p3[[2]],
          p3[[3]],#+rremove("ylab"),
          p3[[4]],
          # p1[[5]]+rremove("ylab"),
          widths = c(4.2,4.2),
          nrow = 2,ncol = 2)
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_scaled_snowcourse_vs_model_elevational.jpg')
ggsave(outname, width = 8, height=8, units='in',dpi = 600)

df %>%
  ggplot()+
  geom_point(aes(x= SWEcm, y = swe_scaledcm, color = Elevation), alpha=0.5)+
  scale_colour_gradientn(colours = myPalette(100), limits=c(min(df$Elevation), max(df$Elevation)))+
  theme(legend.position="top",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=12))
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_scaled_snowcourse_vs_model_elevaruinal_legend.jpg')
ggsave(outname, width = 8, height=8, units='in',dpi = 600)

df %>%
  ggplot()+
  geom_point(aes(x= SWEcm, y = swe_scaledcm, color = as.factor(month)), alpha=1)+
  scale_color_brewer(palette="Spectral")+
  theme(legend.position="top",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size=12))
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_scaled_snowcourse_vs_model_month_legend.jpg')
ggsave(outname, width = 8, height=8, units='in',dpi = 600)








outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/0_allinone/2_vali_sc_ori_v3.jpg'
ggsave(outname, width = 16, height=4, units='in',dpi = 600)

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/2_validation_snow_course/2_vali_sc_600dpi_ori_v3.pdf'
ggsave(outname, width = 16, height=4, units='in',dpi = 600)


#check whether the missing dates were overlaped with snow course obs dates -----
library(DescTools)
if(F){
  fnmissing <- list.files('D:/GoogleDrive/Research/P2_LRM_test/', glob2rx('simdate_all_20200609*txt'), full.names = T)
  date_missing <- data.frame(date=as.Date('20010101','%Y%m%d'))
  for(i in 1:length(fnmissing)){
    date_missing <- rbind(date_missing, cbind.data.frame(date=read.csv(fnmissing[i])[,1]))
  }
  date_missing <- unique(date_missing$date)
  date_snow_course <- unique(sc_data$DateObs)
  date_snow_course <- as.Date(as.character(date_snow_course),'%Y%m%d')
  overlapdate <- date_missing[date_missing %in% date_snow_course]
}

# add missing data to sc file ---------------------------------------------
if(F){
  path_data <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/2_validation_snow_course/data/missing/'
  slist <- c('baseline','FSCA','noREC')
  outlist <- c('LRM_baseline','LRM_FSCA','LRM_noREC')
  for(i in 1:length(idlist)){
    tag <- slist[i]
    fn <- list.files(path_data, glob2rx(paste0('*', tag,'*.txt')), full.names = T)
    mdata <- data.frame()
    for(j in 1:length(fn)){
      data <- read.csv(fn[j])
      mdata <- rbind(mdata, data)
    }
    # mdata <- mdata[order(mdata$Date),]
    outname <- paste0('D:/GoogleDrive/Research/P2_LRM_test/figures/2_validation_snow_course/data/',outlist[i],'_sc.txt')
    write.csv(mdata,outname, row.names = F)
  }
}



#2.1 write SC to one file (has been done in rc)---------------------------------------------------------
# each product, all available station-year
path_data <- 'D:/Data/SWE/dataset/med/ground_ext'
path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/data/med/extract/LRM/'

# -- need to check the location of the data --
i_start <- 1
i_end <- 10

if(F){
  # id <- c('LRM_V1_pl_wfscd','LRM_V1_pl_wfsca','LRM_V1_pl_wofsca')
  id <- c('LRM_V1_pl_wcfsca')
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

