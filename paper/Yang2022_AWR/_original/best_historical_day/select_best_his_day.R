# pick the best historic date
library(spatstat)

path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/bestday/SS1'
fn <- list.files(path,'.txt', full.names = T)
# subfn <- fn[substr(fn, nchar(fn)-7, nchar(fn)-6) %in% c('01','02','03','04','05','06','07','12','11')]
all <- NA
for(i in 190:length(fn)){
  datestr <- substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4)
  date <- substr(fn[i], nchar(fn[i])-7, nchar(fn[i])-6)
  
  # read data     
  # only contains evaluation for dates with the difference between nobsp and the nsimp is less than 10 stations
  oridata <- read.csv(fn[i], stringsAsFactors = F)
  print(summary(as.Date(oridata$hisday,'%Y-%m-%d')))
  oridata <- oridata[as.Date(oridata$hisday,'%Y-%m-%d') < as.Date('20170101','%Y%m%d'),]
  data <- oridata
  # get the number of non-zero station numbers for simulation day
  nobsp <- max(data$nobsp)
  
  # print(data[order(-data$r2),])
  if(nobsp < 10){
    data <- data[1,]
    data[,2:ncol(data)] <- NA
    
  }else{
    # pct <- 0.75
    # 
    # n_min <- round(nobsp * pct, 0)
    # data <- data[data$nsimp > n_min,]
    # 
    # n <- dim(data)[1]
    # pn <- round(n* (1-pct),0)
    # 
    # # data <- data[order(-data$r2),]
    # # data <- data[1,]
    # 
    # # n <- dim(data)[1]
    # # pn <- round(n*(1-pct),0)
    # # 
    # # data <- data[order(data$rmse/data$meanobs),]
    # # data <- data[1:pn,]
    # 
    # if(dim(data)[1] > 9){
    #   n <- dim(data)[1]
    #   pn <- round(n*(1-pct),0)
    #   
    #   data <- data[order(-data$spaef),]
    #   data <- data[1:pn,]
    # }
    
    data <- data[data$nsimp > low & data$nsimp < high,]
    
    data <- data[which.max(data$r2),]
    
    if(length(data)==0){
      data <- oridata[1,]
      data[,2:ncol(data)] <- NA
    } 
    
   
    
  }
  all <- rbind(all, data)
  
}
all <- all[-1,]
print(summary(as.Date(all$hisday,'%Y-%m-%d')))
outname <- 'D:/GoogleDrive/Research/P2_LRM_test/data/bestday/SS_r2_20200609.txt'

write.csv(all, outname, row.names = F)

hisdat <- data.frame(simdate = all$Today, hisdate = all$hisday)
hisdat <- hisdat[complete.cases(hisdat),]
summary(hisdat)
hisdat$simdate <- as.Date(hisdat$simdate,'%Y-%m-%d')
hisdat$hisdate <- as.Date(hisdat$hisdate,'%Y-%m-%d')

write.csv(hisdat, 'D:/GoogleDrive/Research/P2_LRM_test/data/bestday/bestday_r2_20200609.txt', row.names = F)
write.csv(hisdat, 'D:/GoogleDrive/Research/P2_LRM_test/hisdate_SS_r2_20200609.txt', row.names = F)

simall <- data.frame(simdate=hisdat$simdate)
asosim <- read.csv('D:/GoogleDrive/Research/P2_LRM_test/simdate_r2_aso.txt')
sub_sim <- all[all$Today %in% asosim$x,]
sub_sim <- data.frame(simdate = sub_sim$Today, hisdate = sub_sim$hisday)

write.csv(sub_sim, 'D:/GoogleDrive/Research/P2_LRM_test/hisdate_aso_r2_20200328.txt', row.names = F)
write.csv(simall, 'D:/GoogleDrive/Research/P2_LRM_test/simdate_all_r2_20200328.txt', row.names = F)

#need to remove date before 2000-02-24 because there are no fsca data before that date.

library(lubridate)
# check wheather there were missing days 
for(i in (b+1):length(fn)){
  datestr1 <- substr(fn[i-1], nchar(fn[i-1])-11, nchar(fn[i-1])-4)
  datestr2 <- substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4)
  d1 <- as.Date(datestr1,'%Y%m%d')
  d2 <- as.Date(datestr2,'%Y%m%d')

  if(as.numeric(d2-d1) > 1){
    stop(datestr1)
  }
}
i
b <- i



# use the old methods 
# ss_r2 highest
# pick the best historic date
library(spatstat)

path <- 'D:/GoogleDrive/Research/P2_LRM_test/data/bestday/SS1'
path <- 'D:/GoogleDrive/Research/P2_LRM_test/input/res500/SS_stat'
fn <- list.files(path,'.txt', full.names = T)
# subfn <- fn[substr(fn, nchar(fn)-7, nchar(fn)-6) %in% c('01','02','03','04','05','06','07','12','11')]
all <- NA
for(i in 1:length(fn)){
  datestr <- substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4)
  date <- substr(fn[i], nchar(fn[i])-7, nchar(fn[i])-6)
  
  oridata <- read.csv(fn[i], stringsAsFactors = F)
  data <- oridata
  # nobsp <- max(data$nobsp)
  # if(nobsp < 10){
  #   data <- data[1,]
  #   data[,2:ncol(data)] <- NA
  #   
  # }else{
  #   
    data <- data[order(-data$r2),]
    data <- data[1,]
    
    n <- dim(data)[1]
    pn <- round(n*(1-pct),0)
    
    data <- data[order(data$rmse/data$meanobs),]
    data <- data[1:pn,]
    
    if(dim(data)[1] > 9){
      n <- dim(data)[1]
      pn <- round(n*(1-pct),0)
      
      data <- data[order(-data$spaef),]
      data <- data[1:pn,]
    }
    
    data <- data[which.max(data$r2),]
    if(length(data)==0){
      data <- oridata[1,]
      data[,2:ncol(data)] <- NA
    } 
    
    
    
  # }
  all <- rbind(all, data)
  
}
all <- all[-1,]

outname <- 'D:/GoogleDrive/Research/P2_LRM_test/SS_20200510.txt'
write.csv(all, outname, row.names = F)

hisdat <- data.frame(simdate = all$Today, hisdate = all$hisday)
hisdat <- hisdat[complete.cases(hisdat),]
summary(hisdat)
hisdat$simdate <- as.Date(hisdat$simdate,'%Y-%m-%d')
hisdat$hisdate <- as.Date(hisdat$hisdate,'%Y-%m-%d')
write.csv(hisdat, 'D:/GoogleDrive/Research/P2_LRM_test/data/bestday/bestday_20200325.txt', row.names = F)
write.csv(hisdat, 'D:/GoogleDrive/Research/P2_LRM_test/hisdate_SS_20200325.txt', row.names = F)
simall <- data.frame(simdate=hisdat$simdate)
write.csv(simall, 'D:/GoogleDrive/Research/P2_LRM_test/simdate_all_20200325.txt', row.names = F)






