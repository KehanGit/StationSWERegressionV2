
path <- 'E:/GoogleDrive/Research/RT_SWE/input/daily_SS/20010101_20210723'
fn <- list.files(path,'.txt', full.names = T)


# firstly, get the ntarget for each simulation day, if ntarget < 40, than there is no need to run the model ------------
path_root <- 'E:/GoogleDrive/Research/RT_SWE/input/' 
station_data <- read.csv(paste0(path_root,'station_',datestre,'.txt'), stringsAsFactors = F)
station_sum <-
  station_data %>%
  group_by(dte) %>%
  summarise(N = sum(pillowswe >0))

station_sum$month <- substr(station_sum$dte, 5, 6)
station_sum <- station_sum[station_sum$dte >= 20010101,]
station_sum <- station_sum[station_sum$month %in% c('01','02','03','04','05','06'),]
station_sum <- station_sum[station_sum$N > 39,]
summary(station_sum)
dim(station_sum)
write.csv(station_sum,'D:/GoogleDrive/Research/P2_LRM_test/data/0review/besday/num_non_zero_01_21_alldays.txt', row.names = F)

 

# select the best historical day using three different methods
# select the first three days with the highestest r2
threedays <- data.frame()
# select the first ten days with the highested r2 and then use the one with the lowest rmse
tendays <- data.frame()
# select one day with the highest r2
oneday <- data.frame() 
for(i in 1:length(fn)){
  datestr = substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4)
  # if the number of non-zero SWE stations is no less than 40
  if(datestr %in% station_sum$dte){
    data <- read.csv(fn[i])
    data <- data[complete.cases(data),]
    data <- data[order(-data$r2),]
    # need to check the 10% again because when use complete case, record with hist = NA was removed 
    ntarget <- max(data$ntarget)
    
    data <- data[data$ntarget > (ntarget-0.1* ntarget),] # difference within 10%
    subdata <- data[abs(data$ntarget-ntarget) < 0.1* ntarget,]
    subdata$nmax = ntarget
    
    names(subdata)[1] <- 'histday'
    
    threedays <- rbind(threedays, cbind.data.frame(targetday = substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4), subdata[1:3,]))
    oneday <- rbind(oneday, cbind.data.frame(targetday = substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4), subdata[1:1,]))
    tendays <- rbind(tendays, cbind.data.frame(targetday = substr(fn[i], nchar(fn[i])-11, nchar(fn[i])-4), subdata[1:10,]))
    
  }
}


threeback <- threedays
oneback <- oneday
select_one <- tendays %>%
  group_by(targetday) %>%
  summarise(histday=histday[which.min(rmse)], meantarget=meantarget[which.min(rmse)], meanhist=meanhist[which.min(rmse)],
            ntarget = ntarget[which.min(rmse)],nhist = nhist[which.min(rmse)], 
            r2 = r2[which.min(rmse)], rmse2 = rmse[which.min(rmse)], spaef = spaef[which.min(rmse)])


threedays$year <- substr(threedays$targetday,1,4)
threedays$month <- substr(threedays$targetday,5,6)
select_one$month <- substr(select_one$targetday,5,6)

oneday$year <- substr(oneday$targetday,1,4)
oneday$month <- substr(oneday$targetday,5,6)
select_one$month <- substr(select_one$targetday,5,6)


# check missing data

sub_one <- oneday
sub_one <- sub_one[sub_one$month %in% c('01','02','03','04','05','06'),]
sub_one <- sub_one[!is.na(sub_one$r2),]
# sub_one <- sub_one[sub_one$ntarget > 39,]
summary(sub_one)
dim(sub_one) # 2977 simulations
write.csv(sub_one,'D:/GoogleDrive/Research/P2_LRM_test/data/0review/besday/one_maxr2_01_21_alldays.txt', row.names = F)


sub_one <- threedays
sub_one <- sub_one[!is.na(sub_one$r2),]
# sub_one <- sub_one[sub_one$ntarget > 39,]
sub_one <- sub_one[sub_one$month %in% c('01','02','03','04','05','06'),]
#check the dates remain all include three days
summary(sub_one)
a <- sub_one %>%
  group_by(targetday) %>%
  summarise(N = sum(!is.na(ntarget)))
a[a$N < 3,] # NA
dim(sub_one)/3 # 2977 * 3
# 10 days cannot find 3 days 
write.csv(sub_one,'D:/GoogleDrive/Research/P2_LRM_test/data/0review/besday/three_maxr2_01_21_alldays.txt', row.names = F)

sub_one <- select_one
sub_one <- sub_one[sub_one$month %in% c('01','02','03','04','05','06'),]
sub_one <- sub_one[!is.na(sub_one$r2),]
# sub_one <- sub_one[sub_one$ntarget > 39,]
summary(sub_one)
dim(sub_one)
write.csv(sub_one,'D:/GoogleDrive/Research/P2_LRM_test/data/0review/besday/one_10maxr2_minrmse_01_21_alldays.txt', row.names = F)

sum(as.numeric(as.character(sub_one$targetday)) < 20200101)
# 2682 is the number of the old version

# create simulation date list ----------------
write.csv(cbind.data.frame(simdate = sub_one$targetday), 'E:/GoogleDrive/Research/RT_SWE/input/simdate_20210730.txt', row.names = F)

# check missing data 
# 2001-2021 Jan to June 
# run the models with missing data -- only compare their overlaping period
for(i in 2001:2021){
  sub_one <- oneday[oneday$year == i,]
  sub_one <- sub_one[complete.cases(sub_one),]
  sub_one <- sub_one[sub_one$month %in% c('01','02','03','04','05','06'),]
  sub_one <- sub_one[!is.na(sub_one$r2),]
  sub_one <- sub_one[sub_one$ntarget > 39,]
  View(sub_one)
  print(paste0(i,':', dim(sub_one)[1]))
  print(sub_one[dim(sub_one)[1],])
  datestr=sub_one$targetday[dim(sub_one)[1]]
  doy=strftime(as.Date(datestr,'%Y%m%d'),'%j')
  print(paste0('doy =', doy))
}