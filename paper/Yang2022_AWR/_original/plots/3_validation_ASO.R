
library(ggplot2)
library(StationSWERegressionV2)
library(scales)
library(ggpubr)

setwd('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/')


# step 1: plot 4 ASO subplots ---------------------------------------------
if(T){
  # colors <- c('#7F9F00',"#FC4E07", "#00AFBB","#E7B800",'#8106A9')
  colors <- c('#7F9F00',"#FC4E07", "#00AFBB",'#8106A9')
  dfalpha <- 0.7
  dfsize <- 250
  axis_x_size <- 13
  axis_y_size <- 13
  axis_labx_size <- 12
  axis_laby_size <- 12
  legend_size <- 4.2
  
  fn <- list.files('data/','.txt', full.names = T)
  # fn <- fn[-3] # remove no rec
  # pid <-c('LRM-FSCA','LRM-baseline','LRM-noREC','NWM-SWE','SNODAS')
  # lid <-c('LRM-FSCA','LRM-baseline','LRM-noREC','SNODAS','NWM-SWE')
  # pid <-c('LRM-FSCA','LRM-baseline','NWM-SWE','SNODAS')
  lid <-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
  pid = lid
  
  stat_list <- c(expression('R'^2),'RMSE (cm)','RMAE (%)','PBIAS (%)')
  id_list <- c('R2','RMSE','RMAE_new','PBIAS_old')
  
  
  pdata <- data.frame()
  f1 <- list()
  f2 <- list()
  ymins <- c(0,  0, 0,-70)
  ymaxs <- c(1.1, 80, 100,150)
  f1 <- list()
  f2 <- list()
  Alable <- c('A','B','C','D')
  alable <- c('a','b','c','d')
  scales <- c(0.95,0.95,0.95,0.95)
  for(j in 1:length(stat_list)){
    
    print(j)
    pdata <- read.csv(fn[1])
    pdata$RMAE_new <- pdata$MAE / pdata$Mean_obs * 100
    pdata <- pdata[,c('Date', id_list[j])]
    names(pdata)[2]<- pid[1]
    for(i in 2:length(fn)){
      data <- read.csv(fn[i])
      data$RMAE_new <- data$MAE / data$Mean_obs * 100
      data <- data[,c('Date', id_list[j])]
      names(data)[2] <- pid[i]
      pdata <- merge(pdata, data, by = 'Date')
    }
    
    if(F){
      pdata <- pdata[pdata$Date %in% date_qc[,1],]
      dim(pdata)
      summary(pdata)
    }
    all_data <- data.frame()
    for(i in 1:length(fn)){
      all_data <- rbind(all_data, cbind.data.frame(Date=pdata$Date, value = pdata[,pid[i]], id = pid[i]))
    }
    if(id_list[j] == 'RMSE'){
      all_data$value <- all_data$value/10
    }
    # date_qc <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date.txt')
    date_qc <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date_th40.txt')
    # date_qc <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date_th40_v2.txt')
    all_data <- all_data[all_data$Date %in% date_qc[,1],]
    rec_alph <- 0.15
    all_data$label <- strftime(as.Date(as.character(all_data$Date),'%Y%m%d'),'%y-%b')
    print(summary(all_data))
    
    
    all_data$idorder <- NA
    all_data$idorder[all_data$id == 'LRM-baseline'] <- 2
    all_data$idorder[all_data$id == 'LRM-FSCA'] <- 1
    all_data$idorder[all_data$id == 'LRM-noREC'] <- 3
    all_data$idorder[all_data$id == 'SNODAS'] <- 4
    all_data$idorder[all_data$id == 'NWM-SWE'] <- 5
    
    a<-
      all_data %>%
      group_by(id) %>%
      summarize(mean = mean(value), median = median(value), sd = sd(value))
    print(a)
    
    b <- all_data[all_data$id == 'LRM-FSCA',]
    c <- all_data[all_data$id == 'LRM-baseline',]
    d <- b$value-c$value
    e <- cbind.data.frame(id = b$label, d)
    sum(d < 0)
    # }
    
    
    f1[[j]] <-
      # all_data %>%
      all_data %>%
      mutate(id = fct_reorder(id, idorder)) %>%
      # mutate(mth2 = fct_reorder(mth2, order)) %>%
      ggplot()+
      annotate("rect", xmin=as.factor('20130403'), xmax=as.factor('20130429'), ymin = -Inf, ymax = Inf,fill="gray50", alpha=rec_alph) +
      annotate("rect", xmin=as.factor('20140323'), xmax=as.factor('20140502'), ymin = -Inf, ymax = Inf,fill="gray50", alpha=rec_alph) +
      annotate("rect", xmin=as.factor('20150217'), xmax=as.factor('20150325'), ymin = -Inf, ymax = Inf,fill="gray50", alpha=rec_alph) +
      annotate("rect", xmin=as.factor('20160326'), xmax=as.factor('20160416'), ymin = -Inf, ymax = Inf,fill="gray50", alpha=rec_alph) +
      annotate("rect", xmin=as.factor('20170303'), xmax=as.factor('20170604'), ymin = -Inf, ymax = Inf,fill="gray50", alpha=rec_alph) +
      geom_point(aes(x=as.factor(Date), y= value, color = id, shape=id),size=2.5, show.legend = F, alpha=0.8)+
      annotate('text', x = '20130429', y= scales[j]*(ymaxs[j]-ymins[j])+ymins[j], label = Alable[j], size=8, colour = "black", parse = TRUE)+
      # ylab(expression(~R^{"2"}))+
      ylab(stat_list[j])+
      xlab('Date')+
      ylim(c(ymins[j], ymaxs[j]))+
      scale_color_manual(values=colors, labels=lid)+
      scale_shape_manual(values=c(15,16,17,10,7), labels=lid)+
      # scale_x_discrete(breaks=all_data$Date,
      #                  labels=all_data$label)+
      theme(legend.text = element_text(size=legend_size),
            # legend.position = c(0.75,0.8),
            legend.title = element_text(size = legend_size+2, colour = 'black'),
            axis.title.x =element_blank(),
            axis.title.y = element_text(size=axis_y_size, colour = 'black'),
            axis.text.x = element_text(size = axis_labx_size, angle =90,vjust = 0.5, colour = 'black'),
            axis.text.y = element_text(size = axis_laby_size, colour = 'black'),
            panel.grid.major = element_line(colour = "grey90"),
            panel.background = element_blank(),
            panel.border = element_rect(linetype = "solid", fill = NA),
            axis.line = element_line(colour = "black"))
    f2[[j]] <- 
      all_data %>%
      mutate(id = fct_reorder(id, idorder)) %>%
      ggplot()+
      geom_boxplot(aes(x=id, y= value, color = id), show.legend = F, alpha=0.8)+
      annotate('text', x = 'LRM-FSCA', y= scales[j]*(ymaxs[j]-ymins[j])+ymins[j], label = alable[j], size=8, colour = "black", parse = TRUE)+
      ylab(stat_list[j])+
      xlab('')+
      ylim(c(ymins[j], ymaxs[j]))+
      scale_color_manual(values=colors, labels=lid)+
      theme(legend.text = element_text(size=legend_size),
            legend.title = element_text(size = legend_size+2),
            axis.title.x =element_text(size = axis_x_size, colour = 'black'),
            axis.title.y = element_text(size=axis_y_size,colour = 'black'),
            axis.text.x = element_text(size = axis_labx_size, angle =90,vjust = 0.5, colour = 'black'),
            axis.text.y = element_text(size = axis_laby_size, colour = 'black'),
            panel.grid.major = element_line(colour = "grey90"),
            panel.background = element_blank(),
            panel.border = element_rect(linetype = "solid", fill = NA),
            axis.line = element_line(colour = "black")
      )
  }
  
  #orgnize figures
  gfigure <- list()
  for(i in 1:4){
    gfigure[[i]] <- 
      ggarrange(f1[[i]],
                f2[[i]]+rremove("ylab")+rremove('y.text'),
                align = 'h',
                # hjust = -5,
                # vjust = 1.5,
                widths = c(8,2.5),
                nrow = 1,ncol = 2)
    path_fig <- 'D:/GoogleDrive/Research/P2_LRM_test/0submittion/figures'
    # outname <-  paste0('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/',id_list[i],'3.jpg')
    outname <-  paste0(path_fig,'/fig6_validation_ASO/',id_list[i],'.jpg')
    ggsave(outname, width = 6, height=4, units='in',dpi = 600)
  }
  
}

# step2.2 legend ----------------------------------------------------------
all_data %>%
  ggplot()+
  geom_point(aes(x=as.factor(Date), y=all_data$value, color = id, shape=id),size=2.5, show.legend = T, alpha=0.8)+
  ylab(stat_list[j])+
  xlab('Date')+
  ylim(c(ymins[j], ymaxs[j]))+
  scale_color_manual(values=colors, labels=lid)+
  scale_shape_manual(values=c(15,16,17,10,7), labels=lid)+
  theme(legend.key=element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_text(size = 14),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        axis.title.x =element_text(size = axis_x_size),
        axis.title.y = element_text(size=axis_y_size),
        axis.text.x = element_text(size = axis_labx_size, angle =90,vjust = 0.5),
        axis.text.y = element_text(size = axis_laby_size),
        panel.grid.major = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.line = element_line(colour = "black"))
outname <-  paste0('legend_point2.png')
ggsave(outname, width = 5, height=4, units='in',dpi = 600,bg = "transparent")
all_data %>%
  ggplot()+
  geom_boxplot(aes(x=id, y=all_data[,2], color = id), show.legend = T, alpha=0.8)+
  ylab(stat_list[j])+
  xlab('')+
  ylim(c(ymins[j], ymaxs[j]))+
  scale_color_manual(values=colors, labels=lid)+
  theme(legend.key=element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_text(size = 14),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        axis.title.x =element_text(size = axis_x_size),
        axis.title.y = element_text(size=axis_y_size),
        axis.text.x = element_text(size = axis_labx_size, angle =90,vjust = 0.5),
        axis.text.y = element_text(size = axis_laby_size),
        panel.grid.major = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.line = element_line(colour = "black"))
# theme(legend.key=element_blank(),
#       legend.text = element_text(size=13),
#       legend.title = element_text(size = 14),
#       legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#       legend.box.background = element_rect(fill = "transparent"),
#       axis.title.x =element_text(size = axis_x_size),
#       axis.title.y = element_text(size=axis_y_size),
#       axis.text.x = element_text(size = axis_labx_size, angle =90,vjust = 0.5),
#       axis.text.y = element_text(size = axis_laby_size),
#       panel.grid.major = element_line(colour = "grey90"),
#       panel.background = element_rect(fill = "transparent"), # bg of the panel
#       plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#       panel.border = element_rect(linetype = "solid", fill = NA),
#       axis.line = element_line(colour = "black")
# )
outname <-  paste0('Legend_box3.png')
ggsave(outname, width = 2.5, height=4, units='in',dpi = 600,bg = "transparent")




# step 2: summary of data  --------------------------------------------------------
fn <- list.files('data/','.txt', full.names = T)
pid <-c('LRM-FSCA','LRM-baseline','LRM-noREC','NWM-SWE','SNODAS')
stat_list <- c(expression('R'^2),'RMSE (cm)','RMAE (%)','PBIAS (%)')
id_list <- c('R2','RMSE','RMAE_new','PBIAS_old')
pdata <- data.frame()
date_qc <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO/LRM_date_th40.txt')
outdata <- data.frame()
for(j in 1:length(stat_list)){
  print(j)
  pdata <- read.csv(fn[1])
  pdata$RMAE_new <- pdata$MAE / pdata$Mean_obs * 100
  pdata <- pdata[,c('Date', id_list[j])]
  names(pdata)[2]<- pid[1]
  for(i in 2:length(fn)){
    data <- read.csv(fn[i])
    data$RMAE_new <- data$MAE / data$Mean_obs * 100
    data <- data[,c('Date', id_list[j])]
    names(data)[2] <- pid[i]
    pdata <- merge(pdata, data, by = 'Date')
  }
  pdata <- pdata[pdata$Date %in% date_qc[,1],]
  dim(pdata)
  wdata <- data.frame()
  for(i in 1:5){
    wdata <- rbind(wdata, cbind.data.frame(Date = pdata$Date, value = pdata[,i+1], ID = names(pdata)[i+1]))
  }
  sdata <- 
    wdata %>%
    group_by(ID) %>%
    summarise(mean = mean(value), median = median(value), sd = sd(value))
  outdata <- rbind(outdata, sdata)
}

