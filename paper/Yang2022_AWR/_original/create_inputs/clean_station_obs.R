# clean station obse 
# KH 20200419

# plot time series of statin obs, remove the obs with obvious errors--------
if(F)
{
  sp_data <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/stationdata_20200315.txt')
  sitelist <- unique(sp_data$Site_ID)
  library(ggpubr)
  Path_out <- 'D:/GoogleDrive/Research/P2_LRM_test/input/station_fig'
  dfsize = 0.7
  for(i in 1:length(sitelist)){
    print(sitelist[i])
    p1<-ggplot(sp_data[sp_data$Site_ID==sitelist[i],]) +
      geom_line(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe))+
      geom_point(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe), color = 'red', alpha=0.2, size=dfsize)+
      scale_x_date(breaks = "6 months", limits = c(as.Date("2000-01-01"), as.Date("2005-01-01")))+
      theme_bw()
    p2<-ggplot(sp_data[sp_data$Site_ID==sitelist[i],]) +
      geom_line(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe))+
      geom_point(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe), color = 'red', alpha=0.2, size=dfsize)+
      scale_x_date(breaks = "6 months", limits = c(as.Date("2005-01-01"), as.Date("2010-01-01")))+
      theme_bw()
    p3<-ggplot(sp_data[sp_data$Site_ID==sitelist[i],]) +
      geom_line(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe))+
      geom_point(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe), color = 'red', alpha=0.2, size=dfsize)+
      scale_x_date(breaks = "6 months", limits = c(as.Date("2010-01-01"), as.Date("2015-01-01")))+
      theme_bw()
    p4<-ggplot(sp_data[sp_data$Site_ID==sitelist[i],]) +
      geom_line(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe))+
      geom_point(aes(x=as.Date(as.character(dte),'%Y%m%d'), y=pillowswe), color = 'red', alpha=0.2, size=dfsize)+
      scale_x_date(breaks = "6 months", limits = c(as.Date("2015-01-01"), as.Date("2020-01-01")))+
      theme_bw()
    
    
    ggarrange(p1,p2,p3,p4,
              widths = c(5,5),
              heights = c(4,4),
              nrow = 2,ncol = 2)
    outname <- paste0(Path_out,'/original_',sitelist[i],'.jpg')
    ggsave(outname, width = 12, height=6, units='in',dpi = 600)
    
  }
}


# check the data file--------
  # ADM, remove 19990804
  # BCB, need to remove some jump values; the original data file has been labeled 'e' for those jumped obs; 
  # BSH, need to remove some jump values; the original data file has been labeled 'e' for those jumped obs; 
  # CBT has a lot of missing values
  # DAN --> 2009 April 
  # DPO --> 2009 Oct, Nev, Dec, a lot of missing values
  # FRN --> 2003 Oct, 2001 Oct, 2008 Oct
  # HHM --> 2012 Apirl
  # HOR --> 2005 March
  # RBP --> 2003 April
  # SLM --> 2007 July
  # TES --> only 3 obs -- no data --> should remove this station 
  # TUN --> elicit obs around Oct
  # UBC --> many elicit obs 
  # VLC --> 2010 May

path_ori <- 'D:/GoogleDrive/Research/P2_LRM_test/input/snotel/snotel'
id_list <- c('ADM','BCB','BSH','CBT','DAN','DPO','FRN','HHM','HOR','RBP','SLM','TES','TUN','VLC','UBC')
fn_ori <- list.files(path_ori, '.csv', full.names = T)
sub_fn_ori <- fn_ori[substr(fn_ori, nchar(fn_ori)-17, nchar(fn_ori)-15) %in% id_list]
length(id_list)
length(sub_fn_ori)
station_all <- NA # get station data for wy 2001-2019
# flag e cannot be used as a standard to remove data 
# need manually select
# for(i in 1:length(sub_fn_ori)){
#   data <- read.csv(sub_fn_ori[i], stringsAsFactors = F)
#   subdata <- data[as.numeric(substr(data$DATE.TIME,1,4)) > 1999,] # start from 2000-01-01
#   ogz_data <- cbind.data.frame(Site_ID = subdata$STATION_ID, dte = substr(subdata$DATE.TIME,1,8), 
#                                pillowswe = as.numeric(subdata$VALUE) * 0.0254, flag = subdata$DATA_FLAG)
#   e_data <- ogz_data[ogz_data$flag == 'e',]
#   plot(x=ogz_data$dte, y=ogz_data$pillowswe)
#   plot(x=e_data$dte, y = e_data$pillowswe)
# }


#should remove TES
e_num <- NA
station_all <- NA
fn_ori <- fn_ori[substr(fn_ori, nchar(fn_ori)-17, nchar(fn_ori)-15) != 'TES']
length(fn_ori)
for(i in 1:length(fn_ori)){
  data <- read.csv(fn_ori[i], stringsAsFactors = F)
  subdata <- data[as.numeric(substr(data$DATE.TIME,1,4)) > 1999,] # start from 2000-01-01
  ogz_data <- cbind.data.frame(Site_ID = subdata$STATION_ID, dte = substr(subdata$DATE.TIME,1,8), 
                               pillowswe = as.numeric(subdata$VALUE) * 0.0254,
                               flag = subdata$DATA_FLAG)
  e_data <- ogz_data[ogz_data$flag == 'e',]  
  e_num <- rbind(e_num, cbind.data.frame(Site_ID = subdata$STATION_ID[1], num = dim(e_data)[1]))
  r_data <- ogz_data[ogz_data$flag != 'e',]
  station_all <- rbind(station_all, r_data)
}


station_all <- station_all[!is.na(station_all$pillowswe),]
dim(station_all)
summary(station_all)
corloc <- sp_data[,c('Site_ID','Longitude','Latitude')]
corloc <- unique(corloc)

station_all2 <- merge(corloc, station_all,by = 'Site_ID')
write.csv(station_all2,'D:/GoogleDrive/Research/P2_LRM_test/input/snotel/station_20200419_flg_neg.txt', row.names=F)
station_all_backup <- station_all
station_all3 <- station_all2[station_all2$pillowswe>=0,]
num <-
  station_all3 %>%
  group_by(dte) %>%
  summarize(num = sum(!is.na(pillowswe)))

summary(num)
pnum <- cbind.data.frame(dte = num[,1], num = num[,2])
ggplot(num, aes(x=pnum[,1], y= pnum[,2]))+
  geom_point()+
  theme_bw()


numrv <- pnum[pnum$num < 40,]

station_all4 <- station_all3[,-6]
write.csv(station_all4,'D:/GoogleDrive/Research/P2_LRM_test/input/snotel/station_20200419.txt', row.names=F)
