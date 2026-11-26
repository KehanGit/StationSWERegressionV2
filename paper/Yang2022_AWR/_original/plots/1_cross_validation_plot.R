# results 1. cross-validation 
# time series 
# KH 20200507

library(ggplot2)
library(StationSWERegressionV2)
library(scales)
library(ggpubr)

source('D:/GoogleDrive/Github/R/0usefulltools/usefulltools/LRM_qc.R') # no outliner will be removed


getwy = function(datestr){
  yr <- as.numeric(substr(datestr, 1,4))
  month <- substr(datestr, 5,6)
  yr[as.numeric(month) > 9] = yr[as.numeric(month) >9]+1
  return(yr)
}


path_root <- 'D:/Data/SWE/dataset/med/LRM' # the LRM output folder 
path_qc <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/data/v2'
path_out <- paste0(path_qc,'/QC/') # where the statistic file stored

# step 1: write all cv results to one file for each model run without removing any outliners --------------------

step1 == F
if(step1 == T){
  
  fnall <- list.files(path_qc,'.txt', full.name=T)
  fnallname <- list.files(path_qc,'.txt', full.name=F)
  if(length(fnall==0)){
    # if the summaried files were not output yet
    th_cf <- NA
    th_ml_pn <- NA # the threshold for postive station SWE, but both the threshold are not used here
    tag <- c('20200509_LRM_V1_pl_wofsca','20200509_LRM_V1_pl_wfsca_dm','20200512_LRM_V1_pl_wfsca_norec')
    ID  <- c('LRM-baseline','LRM-FSCA','LRM-noREC')
    meg_data <- data.frame()
    for(i in 1:length(tag)){
      df <- LRM_qc(path_root,tag[i], path_out, th_cf, th_ml_pn)
      meg_data <- rbind(meg_data, cbind.data.frame(df, ID = ID[i]))
    }
    head(meg_data)
    
  }else{
    ID <- sapply(strsplit(fnallname,'.t'), '[', 1)
    meg_data <- data.frame()
    for(i in 1:length(fnall)){
      data <- read.csv(fnall[i])
      meg_data <- rbind(meg_data, cbind.data.frame(data, ID = ID[i]))
    }
    head(meg_data)
  }
}

# step 2: remove the simulations with less than 40 (includes 40) positive simulations----
step2 = F
if(step2 == T){
  qc_data <- meg_data
  boxplot(qc_data$model)
  dim(qc_data)
  
  # only remain the simulations on Jan - June
  qc_data$month <- substr(qc_data$Date, 5, 6)
  qc1 <- qc_data[qc_data$month %in% c('01','02','03','04','05','06'),]
  summary(qc1)
  dim(qc1)
  # OutVals = boxplot(qc1$model)$out # the outlier is about 61
  # summary(OutVals)
  
  # we used 40 as the threshold
  qc2 <- qc1[qc1$model > 40,]
  round(nrow(qc2)/nrow(qc1)*100,1) #96.4%
  plot(qc2$month)
  write.csv(qc2, paste0(path_out,'qc_th_model_40_v2.txt'), row.names = F)
  # get the dates overlap with the ASO_date
  path_aso <- 'D:/GoogleDrive/Research/P2_LRM_test/figures/3_validation_ASO'
  date_aso <- read.csv(paste0(path_aso,'/ASO_date.txt'))
  a <- qc2[qc2$Date %in% date_aso[,1],]
  summary(a)
  date_overlap <- a$Date[a$ID == ID[1]]
  for(i in 2:length(ID)){
    date_tem <- a$Date[a$ID == ID[i]]
    date_overlap <- date_overlap[date_overlap %in% date_tem]
  }
  length(date_overlap)
  write.csv(date_overlap, paste0(path_aso,'/LRM_date_th40_v2.txt'), row.names=F)
}

# step 3: plot the cv results -----
step3 =T
if(step3==T){
  # get the overlap dates
  qc2 <- read.csv(paste0(path_out,'qc_th_model_40_v2.txt'), stringsAsFactors =F)
  qc2 <- qc2[complete.cases(qc2),]
  summary(qc2)
  qc3 <- qc2
  qc3$rmse <- qc3$rmse * 100 # change unite to cm
  # date_noREC <- qc3[qc3$ID == 'LRM_noREC',]
  date_FSCA <- qc3[qc3$ID == 'LRM_FSCA',]
  date_baseline <- qc3[qc3$ID == 'LRM_baseline',]
  alldate <- merge(date_FSCA, date_baseline, by = 'Date')
  # alldate <- merge(alldate, date_FSCA, by = 'Date')
  dim(alldate) # 2650 simulations for all three runs
  # if only FSCA and baseline, then 2682 simulations
  
  # plot the cv results
  # colors <- c("#FC4E07",'#7F9F00',"#00AFBB")
  colors <- c('#7F9F00',"#FC4E07")
  dfalpha <- 0.7
  dfsize <- 250
  axis_x_size <- 13
  axis_y_size <- 13
  axis_labx_size <- 12
  axis_laby_size <- 12
  legend_size <- 4.2
  
  # pid <-c('LRM-FSCA','LRM-baseline','LRM-noREC')
  pid <-c('LRM-FSCA','LRM-baseline')
  stat_list <- c(expression('R'^2),'RMSE (cm)','RMAE (%)','Basin-wide PBIAS (%)')
  id_list <- c('r2','rmse','pctmae','pbias')
  ymins <- c(0.25, 0, 20, -10)
  ymaxs <- c(0.80, 55, 175, 50)
  labellist <- c('a','b','c','d')
  f <- list()
  f1 <- list()
  meg_data <- alldate
  # summarize the statistic matrics 
  for(i in 1:length(stat_list)){
    p_data <- rbind(
      # cbind.data.frame(Date = meg_data[,"Date"], value = meg_data[,paste0(id_list[i],'.x')],id='norec'),
      cbind.data.frame(Date = meg_data[,"Date"], value = meg_data[,paste0(id_list[i],'.y')],id='baseline'),
      cbind.data.frame(Date = meg_data[,"Date"], value = meg_data[,paste0(id_list[i],'.x')],id='fsca'))
    print(p_data %>% group_by(id) %>% summarise(mean=mean(value), median = median(value), sd = sd(value)))
  }
  
  for(i in 1:length(stat_list)){
    p_data <- rbind(
      # cbind.data.frame(Date = meg_data[,"Date"], value = meg_data[,paste0(id_list[i],'.x')],id='norec'),
      cbind.data.frame(Date = meg_data[,"Date"], value = meg_data[,paste0(id_list[i],'.y')],id='baseline'),
      cbind.data.frame(Date = meg_data[,"Date"], value = meg_data[,paste0(id_list[i],'.x')],id='fsca'))
    summary(p_data)
    
    p_data$datestr <- p_data$Date
    p_data$Date <- as.Date(as.character(p_data$Date), '%Y%m%d')
    p_data$wy <- getwy(as.character(p_data$datestr))
    p_data$mth <- substr(p_data$datestr, 5,6)
    mth_sum <- p_data %>%
      group_by(mth) %>%
      summarise(N = sum(!is.na(value)))
    print(mth_sum)
    print(round(mth_sum$N/2/19))
    
    ppdata <- p_data
    ppdata <- ppdata[ppdata$mth %in% c('01', '02', '03','04','05','06'),]
    ppdata$order <- as.numeric(ppdata$mth)
    ppdata$idorder[ppdata$id == 'baseline'] <- 2
    ppdata$idorder[ppdata$id == 'fsca'] <- 1
    ppdata$idorder[ppdata$id == 'norec'] <- 3
    ppdata <- ppdata[ppdata$id != 'norec',]
    ppdata$mth2 <- strftime(ppdata$Date, '%b') 
    sumpid  <- 
      ppdata %>%
      group_by(id) %>%
      summarise(mean=mean(value), median=median(value), sd = sd(value))
    print(sumpid)
    print(summary(ppdata))
    
    # t.test
    df <- cbind.data.frame(fsca = ppdata$value[ppdata$id=='fsca'],
                           baseline = ppdata$value[ppdata$id=='baseline'],
                           month = substr(ppdata$Date,6,7)[1:(nrow(ppdata)/2)])
    df$class <- NA
    df$class[df$month %in% c('01','02','03','04')] <- 'A'
    df$class[df$month %in% c('05','06')] <- 'M'
    
    # t.test(df)
    
    if(i!=3){
      f[[i]] <-
        ppdata %>%
        mutate(mth2 = fct_reorder(mth2, order), id = fct_reorder(id, idorder)) %>%
        ggplot()+
        geom_boxplot(aes(x=mth2, y=value, fill = id), show.legend =F, alpha=0.8, outlier.color = NA)+
        # annotate('text', x = 'Jan', y= 0.9*ymaxs[i], label = labellist[i], size=8, colour = "black", parse = TRUE)+
        ylab(stat_list[i])+
        xlab('Month')+
        ylim(c(ymins[i], ymaxs[i]))+
        scale_fill_manual(values=colors, labels=pid)+    
        theme_bw()+
        theme(legend.text = element_text(size=legend_size, color = 'black'),
              legend.title = element_text(size = legend_size+2, color = 'black'),
              axis.title.x =element_text(size = axis_x_size, color = 'black'),
              axis.title.y = element_text(size=axis_y_size, color = 'black'),
              axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5, color = 'black'),
              axis.text.y = element_text(size = axis_laby_size, color = 'black'),
              panel.background = element_blank(),
              panel.border = element_rect(linetype = "solid", fill = NA),
              axis.line = element_line(colour = "black"))
      
      
    }else{
      f[[i]] <-
        ppdata %>%
        mutate(mth2 = fct_reorder(mth2, order), id = fct_reorder(id, idorder)) %>%
        ggplot()+
        geom_boxplot(aes(x=mth2, y=value, fill = id), show.legend =T, alpha=0.8, outlier.color = NA)+
        # annotate('text', x = 'Jan', y= 0.9*ymaxs[i], label = 'c', size=8, colour = "black", parse = TRUE)+
        ylab(stat_list[i])+
        xlab('Month')+
        ylim(c(ymins[i], ymaxs[i]))+
        scale_fill_manual(values=colors, labels=pid)+    
        theme_bw()+
        theme(legend.text = element_text(size=12),
              legend.position = c(0.45,0.8),
              legend.title = element_blank(),
              legend.background = element_blank(),
              axis.title.x =element_text(size = axis_x_size, color = 'black'),
              axis.title.y = element_text(size=axis_y_size, color = 'black'),
              axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5, color = 'black'),
              axis.text.y = element_text(size = axis_laby_size, color = 'black'),
              panel.background = element_blank(),
              panel.border = element_rect(linetype = "solid", fill = NA),
              axis.line = element_line(colour = "black"))
    }
    
    
    
    # ggplot(df)+
    #   geom_point(aes(x = df$fsca, y = df$baseline, color = df$class), alpha = 0.3)+
    #   geom_abline(slope = 1, intercept = 0, color = 'red')
    # 
    # 
    
  }
  
  ggarrange(f[[1]]+rremove("xlab"),
            f[[2]]+rremove("xlab"),
            f[[3]],
            f[[4]],
            heights = c(4,4.2),
            widths = c(4.2,4),
            nrow = 2,ncol = 2)
}


outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/0_cv_all_20210721.jpg'
ggsave(outname, width = 10, height=6, units='in',dpi = 600)

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/0_allinone/0_cv_all_202101278.jpg'
ggsave(outname, width = 10, height=6, units='in',dpi = 600)

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/0_cv_all_020210127_600dpi.pdf'
ggsave(outname, width = 10, height=6, units='in',dpi = 600)

