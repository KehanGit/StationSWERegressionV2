# 0.load libraries --------------------------------------------------------

library(ggplot2)
library(raster)
library(mosaic)
library(forcats)
dfalpha <- 0.1
dfsize <- 250
axis_x_size <- 13
axis_y_size <- 13
axis_labx_size <- 12
axis_laby_size <- 12
legend_size <- 4.2

setwd('D:/GoogleDrive/Research/P2_LRM_test/Stat/peak_stat')
fn <- list.files('./', '.txt', full.names = T)
fnname <- list.files('./', '.txt', full.names = F)
id <- sapply(strsplit(fnname,"_"),'[', 1)
wdata <- data.frame()
for(i in 1:length(fn)){
  data <- read.csv(fn[i])
  wdata <- rbind(wdata, cbind.data.frame(WY = data$dates, PeakSWE = data$SM_mean, id = id[i]))
}
wdata$PeakSWE[wdata$id=='NWM'] <- wdata$PeakSWE[wdata$id=='NWM']
tem <- raster('D:/GoogleDrive/Research/P2_LRM_test/data/template/LRM_SNM_tem/SNM_500.tif')
Npixel <- sum(!is.na(tem[]))
Area <- Npixel*500*500/1000/1000
wdata$Volume <- wdata$PeakSWE*Area/1000/1000

wdata$pid <- NA
wdata$pid[wdata$id=='baseline'] <- 'LRM-baseline'
wdata$pid[wdata$id=='FSCA'] <- 'LRM-FSCA'
wdata$pid[wdata$id=='noREC'] <- 'LRM-noREC'
wdata$pid[wdata$id=='NWM'] <- 'NWM-SWE'
wdata$pid[wdata$id=='SNODAS'] <- 'SNODAS'
wdata$porder <- NA
wdata$porder[wdata$id=='baseline'] <- 2
wdata$porder[wdata$id=='FSCA'] <- 1
wdata$porder[wdata$id=='noREC'] <- 3
wdata$porder[wdata$id=='NWM'] <- 5
wdata$porder[wdata$id=='SNODAS'] <- 4
pid<-c('LRM-FSCA','LRM-baseline','SNODAS','NWM-SWE')
# pid<-c('LRM-FSCA','LRM-baseline','LRM-noREC','SNODAS','NWM-SWE')

pwdata <- wdata[wdata$WY>2003,]
pwdata <- pwdata[pwdata$WY<2018,]
Meanstorage <- 
  pwdata %>%
  group_by(id) %>%
  summarise(mean=mean(Volume), sd = sd(Volume))
# wdata$WY[wdata$id == 'NWM'] <- wdata$WY[wdata$id == 'NWM']-1
colors <- c('#7F9F00',"#FC4E07", "#00AFBB",'#8106A9')
# remove LRM-noREC
wdata <- wdata[wdata$id != 'noREC',]
wdata %>%
  mutate(pid = fct_reorder(pid, porder)) %>%
  ggplot()+
  # geom_smooth(aes(x=WY, y=Volume,group=pid,color = pid, fill = pid),level = 0.95, alpha=0.1, method = 'lm')+
  annotate("rect", xmin=2011.5, xmax=2016.5, ymin = -Inf, ymax = Inf,fill="gray50", alpha=0.4) +
  annotate("text", x=2014, y=60 ,label = "2012-2016 drought") +
  geom_point(aes(x=WY, y=Volume, color = pid, shape = pid), show.legend =T, alpha=0.8, size=2)+
  # ylim(c(0,80))+
  geom_line(aes(x=WY, y=Volume, color = pid), show.legend =T, alpha=0.8, size=0.5)+
  scale_color_manual(values=colors, labels=pid)+
  scale_fill_manual(values=colors, labels=pid)+
  scale_shape_manual(values=c(15,16,17,10,7), labels=pid)+
  ylab('Maximum snow water storage (Gt)')+
  xlab('Water Year')+
  scale_x_continuous(breaks = seq(min(wdata$WY), max(wdata$WY), by = 2))+
  # ylim(c(ymins[i], ymaxs[i]))+
  # scale_fill_manual(values=colors, labels=pid)+    
  # scale_color_manual(values=colors, labels=pid)+
  theme_bw()+
  theme(legend.text = element_text(size=9),
        legend.position = c(0.1,0.82),
        legend.title = element_blank(),
        legend.background = element_blank())
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_line_v2.jpg'
# outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/1_cross_validation_legend_dpi600.jpg'
ggsave(outname, width = 8, height=4, units='in',dpi = 600)

outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/0_allinone/peak_snow_storage_line_v2.jpg'
ggsave(outname, width = 8, height=4, units='in',dpi = 600)
write.csv(wdata,'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_v2.csv', row.names = F)


# model inter-compare
cid <- id[-3]
cdata <- wdata[wdata$id == cid[1],]
names(cdata)[4] <- cid[1] 
for(i in 2:4){
  tmp <- wdata[wdata$id == cid[i],]
  cdata <- merge(cdata, tmp[,c('WY','Volume')], by = 'WY')
  names(cdata)[i+5] <- cid[i] 
}
cdata <- cdata[,c(1,4,7:9)]
oid <- c('FSCA','baseline','SNODAS','NWM')
cdata$fsca_baselie <- cdata$FSCA - cdata$baseline
cdata$fsca_snodas <- cdata$FSCA - cdata$SNODAS
cdata$fsca_nwm <- cdata$FSCA - cdata$NWM
cdata$baseline_snodas <- cdata$baseline - cdata$SNODAS
cdata$baselie_nwm <- cdata$baseline - cdata$NWM
cdata$snodas_nwm <- cdata$SNODAS - cdata$NWM

labels <- c('LRM-FSCA - LRM-baseline','LRM-FSCA - SNODAS','LRM-FSCA - NWM-SWE',
            'LRM-baseline - SNODAS','LRM-baseline - NWM-SWE','SNODAS - NWM-SWE')
boxplot(cdata[,c(6:11)])
pcdata <- data.frame()
for(i in 1:6){
  pcdata <- rbind(pcdata, cbind.data.frame(WY=cdata[,1],storage = cdata[,i+5], id = labels[i]))
}
summary(cdata)
asize = 5
ay = 24
ahjust <- 0.5
ggplot(pcdata,aes(x = id, y = storage, color = id))+
  geom_boxplot(show.legend = F,notch = F)+
  geom_point(show.legend = F)+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="black", alpha = 0.7) +
  geom_hline(yintercept=0,linetype = 'dotted', col = 'black')+
  annotate(geom = 'text', x= 1, y = ay , label='Max: 3.9 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 1, y = ay-2 , label='Mean: 1.9 ± 1.3 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 1, y = ay-4 , label='Min: 0.3 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 2, y = ay , label='Max: 8.4 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 2, y = ay-2 , label='Mean: 2.5 ± 1.9Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 2, y = ay-4 , label='Min: 0.0 Gt', size = asize, hjust = ahjust, fontface = "bold")+
  annotate(geom = 'text', x= 3, y = ay , label='Max: 13.9 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 3, y = ay-2 , label='Mean: 6.3 ± 3.9Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 3, y = ay-4 , label='Min: 0.5 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 4, y = ay , label='Max: 12.2 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 4, y = ay-2 , label='Mean: 3.1 ± 3.1 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 4, y = ay-4 , label='Min: 0.4 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 5, y = ay , label='Max: 17.7 Gt', size = asize, hjust = ahjust, fontface = "bold")+
  annotate(geom = 'text', x= 5, y = ay-2 , label='Mean: 8.1 ± 4.9 Gt', size = asize,  hjust = ahjust)+
  annotate(geom = 'text', x= 5, y = ay-4 , label='Min: 0.8 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 6, y = ay , label='Max: 11.6 Gt', size = asize, hjust = ahjust)+
  annotate(geom = 'text', x= 6, y = ay-2 , label='Mean: 6.1 ± 2.8 Gt', size = asize,  hjust = ahjust)+
  annotate(geom = 'text', x= 6, y = ay-4 , label='Min: 2.4 Gt', size = asize,  hjust = ahjust)+
  ylab('Difference of maximum snow water storage (Gt)')+
  xlab('')+
  ylim (c(-5, 25))+
  # theme_classic()+
  theme(legend.text = element_text(size=legend_size),
        legend.position = c(0.1,0.7),
        legend.title = element_text(size = legend_size+2, color = 'black'),
        axis.title.x =element_text(size = axis_x_size, color = 'black'),
        axis.title.y = element_text(size=axis_y_size, color = 'black'),
        axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5, color = 'black'),
        axis.text.y = element_text(size = axis_laby_size, color = 'black'),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(colour = "grey90"),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.line = element_line(colour = "black"))
outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/6_peakSWE/peak_snow_storage_difference.jpg'
# outname <-  'D:/GoogleDrive/Research/P2_LRM_test/figures/1_cross_validation/1_cross_validation_legend_dpi600.jpg'
ggsave(outname, width = 14, height=5, units='in',dpi = 600)



pdata %>%
  mutate(mth2 = fct_reorder(mth2, order), id = fct_reorder(id, idorder)) %>%
  ggplot()+
  geom_boxplot(aes(x=mth2, y=value, fill = id), show.legend =F, alpha=0.8, outlier.color = NA)+
  # ylab(expression(~R^{"2"}))+
  ylab(stat_list[i])+
  xlab('Month')+
  ylim(c(ymins[i], ymaxs[i]))+
  # scale_fill_manual(values=colors, labels=pid)+   
  scale_color_manual(values=colors, labels=id)+
  scale_shape_manual(values=c(15,16,17,10,7), labels=pid)+
  # scale_color_manual(values=colors, labels=pid)+
  theme_bw()+
  theme(legend.text = element_text(size=legend_size),
        legend.position = c(0.1,0.7),
        legend.title = element_text(size = legend_size+2),
        axis.title.x =element_text(size = axis_x_size),
        axis.title.y = element_text(size=axis_y_size),
        axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5),
        axis.text.y = element_text(size = axis_laby_size),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(colour = "grey90"),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.line = element_line(colour = "black")
  )
pwdata <- wdata[wdata$WY>2003,]
pwdata <- pwdata[pwdata$WY<2018,]
Meanstorage <- 
  pwdata %>%
  group_by(id) %>%
  summarise(mean=mean(Volume), sd = sd(Volume))

pwdata %>%
  # mutate(mth2 = fct_reorder(mth2, month), id = fct_reorder(id, idorder)) %>%
  ggplot()+
  geom_boxplot(aes(x=id, y=Volume, fill = id), show.legend =T, alpha=0.8)+
  # ylab(expression(~R^{"2"}))+
  ylab('Peak snow storage (Gt)')+
  xlab('Month')+
  # ylim(c(ymins[i], ymaxs[i]))+
  # scale_fill_manual(values=colors, labels=pid)+    
  # scale_color_manual(values=colors, labels=pid)+
  theme_bw()
  # theme(legend.text = element_text(size=10),
  #       legend.position = c(0.85,0.8),
  #       legend.title = element_blank(),
  #       axis.title.x =element_text(size = axis_x_size),
  #       axis.title.y = element_text(size=axis_y_size),
  #       axis.text.x = element_text(size = axis_labx_size, angle =0,vjust = 0.5),
  #       axis.text.y = element_text(size = axis_laby_size),
  #       # panel.grid.major = element_blank(),
  #       # panel.grid.minor = element_blank(),
  #       # panel.grid.major = element_line(colour = "grey90"),
  #       panel.background = element_blank(),
  #       panel.border = element_rect(linetype = "solid", fill = NA),
  #       axis.line = element_line(colour = "black")
  # )