# results 1. cross-validation 
# time series 
# KH 20200507

library(ggplot2)
library(StationSWERegressionV2)
library(scales)
library(ggpubr)
library(hydroGOF)

source('D:/GoogleDrive/Github/R/0usefulltools/usefulltools/LRM_qc.R') # no outliner will be removed


getwy = function(datestr){
  yr <- as.numeric(substr(datestr, 1,4))
  month <- substr(datestr, 5,6)
  yr[as.numeric(month) > 9] = yr[as.numeric(month) >9]+1
  return(yr)
}
path_fig <- 'D:/GoogleDrive/Research/P2_LRM_test/data/0review/figures'
# cv -------------------------
path_root <- 'D:/GoogleDrive/Research/P2_LRM_test/data/0review/cross_validation' # the LRM output folder 
tags <- c('20210729_One_GLM_ND_DMFSCA.txt','20210729_One_GLMNET_ND_DMFSCA.txt')
ids <- c('GLM','GLMNET')
all <- data.frame()
fn <- list.files(path_root, glob2rx(paste0('*', tags[1])), full.names = T)
data <- read.csv(fn)
data$ID <- ids[1]
all <- data
for(i in 2:length(tags)){
  fn <- list.files(path_root, glob2rx(paste0('*', tags[i])), full.names = T)
  data <- read.csv(fn)
  data$ID <- ids[i]
  print(dim(data))
  all <- merge(all, data, by = 'Date')
}
dates <- all$Date


# [1] 2962    8
# [1] 2960    8
# for 2001-2019
dates <- dates[substr(dates,1,4) <= 2019]
length(dates)
# there are a few days that are different
alldata_cv <- all[all$Date %in% dates,]

t.test(alldata_cv$r2.x, alldata_cv$r2.y)
median(alldata_cv$r2.x)
mean(alldata_cv$r2.y)

alldata_cv %>%
  ggplot(aes(x= r2.x, y = r2.y))+
  geom_point(color = 'black', alpha=0.25)+
  # geom_smooth(method = 'lm', color = 'red')+
  geom_abline(slope = 1, intercept = 0,color = 'black', size=1)+
  xlab('R-squared for Modeled SWE - GLM')+
  ylab('R-squared for Modeled SWE - GLMNET')+
  theme_bw()

outname <-  paste0(path_fig,'/cv_GLM_GLMNET_r2_significant_better.jpg')
ggsave(outname, width = 6, height=6, units='in',dpi = 600)



t.test(alldata_cv$rmse.x, alldata_cv$rmse.y)
alldata_cv %>%
  ggplot(aes(x= rmse.x, y = rmse.y))+
  geom_point(color = 'black', alpha=0.25)+
  # geom_smooth(method = 'lm', color = 'red')+
  geom_abline(slope = 1, intercept = 0,color = 'black', size=1)+
  xlab('RMSE (cm) for Modeled SWE - GLM')+
  ylab('RMSE (cm) for Modeled SWE - GLMNET')+
  theme_bw()

outname <-  paste0(path_fig,'/cv_GLM_GLMNET_rmse_notsignificant.jpg')
ggsave(outname, width = 6, height=6, units='in',dpi = 600)

t.test(abs(alldata_cv$pbias.x), abs(alldata_cv$pbias.y))
alldata_cv %>%
  ggplot(aes(x= pbias.x, y = pbias.y))+
  geom_point(color = 'black', alpha=0.25)+
  xlim(c(0,50))+
  ylim(c(0,50))+
  # geom_smooth(method = 'lm', color = 'red')+
  geom_abline(slope = 1, intercept = 0,color = 'black', size=1)+
  xlab('PBIAS (%) for Modeled SWE - GLM')+
  ylab('PBIAS (%) for Modeled SWE - GLMNET')+
  theme_bw()

outname <-  paste0(path_fig,'/cv_GLM_GLMNET_pbias_notsignificant.jpg')
ggsave(outname, width = 6, height=6, units='in',dpi = 600)

# sc validation----------------

path_root <- 'D:/GoogleDrive/Research/P2_LRM_test/data/0review/snow_course/Extract_one/' # the LRM output folder 
tags <- c('20210729_One_GLM_ND_DMFSCA_sc.txt','20210729_One_GLMNET_ND_DMFSCA_sc.txt')
# ids <- c('Three','One')
all <- data.frame()

for(i in 1:length(tags)){
  fn <- list.files(path_root, glob2rx(paste0('*', tags[i])), full.names = T)
  data <- read.csv(fn)
  data$ID <- ids[i]
  print(dim(data))
  all <- rbind(all, data)
}
all_sc <- all
pp1 <- list()
colors <- c("#1A8E9C",'#FCA106')
for(i in 1:length(ids)){
  df <- cbind.data.frame(SWE = all_sc$sc_fscascaled[all_sc$ID ==ids[i]],swe_scaled = all_sc$swe_wfsca[all_sc$ID == ids[i]])
  
  r <- cor.test(df$SWE, df$swe_scaled)
  r <- as.numeric(r$estimate)
  r <- r * r
  # rlist <- rbind(rlist, r)
  r <- format(round(r, 2), nsmall = 2)
  
  
  rmae <- hydroGOF::mae(df$SWE, df$swe_scaled)/mean(df$SWE, na.rm=T)*100
  rmae <-  format(round(rmae, 1), nsmall = 1)
  rmae <- paste0(rmae, ' %')
  
  rmse <- hydroGOF::rmse(df$SWE, df$swe_scaled)/10
  rmse <-  format(round(rmse, 1), nsmall = 1)
  rmse <- paste0(rmse, ' cm')
  
  slope <- lm(df$SWE ~ df$swe_scaled)
  slope <- as.numeric(slope$coefficients[2])
  slope <-  format(round(slope, 2), nsmall = 2)
  
  pbias <- (sum(df$swe_scaled - df$SWE))/(sum(df$SWE)) * 100
  # pbiaslist <- rbind(pbiaslist, pbias)
  pbias <- format(round(pbias, 1), nsmall = 1)
  pbias <- paste0(pbias,' %')
  
  xloc <- 120
  yloc <- 50
  yint <- 15
  
  df$SWEcm <- df$SWE/10
  df$swe_scaledcm <- df$swe_scaled/10
  
  
  pp1[[i]] <-
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
    # annotate('text', x = 20, y= 180, label = labels[i], size=8, colour = "black", parse = TRUE)+
    # smoothScatter()+
    geom_smooth(method='lm', color = 'red', size = 1)+
    # 0 means left-justified, 0.5 means centered, and 1 means right-justified.
    annotate('text', x =xloc, y = yloc, label = paste("R^2: ",as.character(r)) ,hjust = 0, size = legend_size, parse=TRUE, color = 'black')+
    annotate('text', x =xloc, y = yloc-yint, label = paste('RMSE:', as.character(rmse)),hjust = 0,size = legend_size, color = 'black')+
    annotate('text', x =xloc, y = yloc-2*yint, label = paste('RMAE:', as.character(rmae)),hjust = 0,size = legend_size, color = 'black')+
    annotate('text', x =xloc, y = yloc-3*yint, label = paste('PBIAS:', as.character(pbias)),hjust = 0,size = legend_size, color = 'black')+
    # annotate('text', x =250, y = 1640, label = paste('UE:', as.character(underestimation),'%'))+
    ylim(c(0,200))+
    xlim(c(0,200))+
    ylab(paste('Modeled SWE - ',pid[i],'(cm) '))+
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
}

# use the original validation results
ggarrange(pp1[[2]],
          p1[[1]],
          widths = c(4.2,4.2),
          nrow = 1,ncol = 2)

# path_fig <- 'D:/GoogleDrive/Research/P2_LRM_test/0submittion/figures'
outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_GLM_GLMNET.jpg')
# outname <-  paste0(path_fig,'/fig5.snowcourse_vali2_scaled_snowcourse_vs_model.jpg')
ggsave(outname, width = 8, height=4, units='in',dpi = 600)


# validation using ASO data ----------------

path_root <- 'D:/GoogleDrive/Research/P2_LRM_test/data/0review/ASO/stat/' # the LRM output folder 
tags <- c('20210729_Thr_GLMNET_ND_DMFSCA_ASO_vali.txt','20210729_One_GLMNET_ND_DMFSCA_ASO_vali.txt')
ids <- c('Three','One')
all <- data.frame()
fn <- list.files(path_root, glob2rx(paste0('*', tags[1])), full.names = T)
data <- read.csv(fn)
data$ID <- ids[1]
all <- data
for(i in 2:length(tags)){
  fn <- list.files(path_root, glob2rx(paste0('*', tags[i])), full.names = T)
  data <- read.csv(fn)
  data$ID <- ids[i]
  print(dim(data))
  all <- merge(all, data, by = 'Date')
}
dates <- all$Date


all_aso <- all[all$Date %in% dates,]
t.test(all_aso$R2.x, all_aso$R2.y)

all_aso %>%
  ggplot(aes(x= R2.x, y = R2.y))+
  geom_point(color = 'black', alpha=1)+
  # geom_smooth(method = 'lm', color = 'red')+
  geom_abline(slope = 1, intercept = 0,color = 'black', size=1)+
  xlab('R-squared for Modeled SWE - GLM')+
  ylab('R-squared for Modeled SWE - GLMNET')+
  theme_bw()

outname <-  paste0(path_fig,'/aso_GLM_GLMNET_r2_notsignificant.jpg')
ggsave(outname, width = 6, height=6, units='in',dpi = 600)



t.test(all_aso$RMSE.x, all_aso$RMSE.y)
all_aso %>%
  ggplot(aes(x= RMSE.x, y = RMSE.y))+
  geom_point(color = 'black', alpha=1)+
  # geom_smooth(method = 'lm', color = 'red')+
  geom_abline(slope = 1, intercept = 0,color = 'black', size=1)+
  xlab('RMSE (cm) for Modeled SWE - GLM')+
  ylab('RMSE (cm) for Modeled SWE - GLMNET')+
  theme_bw()

outname <-  paste0(path_fig,'/aso_GLM_GLMNET_rmse_notsignificant.jpg')
ggsave(outname, width = 6, height=6, units='in',dpi = 600)

t.test(all_aso$PBIAS.x, all_aso$PBIAS.y)
all_aso %>%
  ggplot(aes(x= PBIAS.x, y = PBIAS.y))+
  geom_point(color = 'black', alpha=1)+
  # geom_smooth(method = 'lm', color = 'red')+
  geom_abline(slope = 1, intercept = 0,color = 'black', size=1)+
  xlab('PBIAS (%) for Modeled SWE - GLM')+
  ylab('PBIAS (%) for Modeled SWE - GLMNET')+
  theme_bw()

outname <-  paste0(path_fig,'/aso_GLM_GLMNET_pbias_notsignificant.jpg')
ggsave(outname, width = 6, height=6, units='in',dpi = 600)


