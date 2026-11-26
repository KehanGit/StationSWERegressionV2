library(lubridate)
library(hydroGOF)

#' download and combine station data
#' @param data
#' @export
#'
extract_r2 <- function(data){
  nasim <- table(data$todayswe>0)["TRUE"]
  naobs <- table(data$pillowswe>0)["TRUE"]

  if(!is.na(nasim) && !is.na(naobs) && abs(nasim-naobs)<10){
    data <- data[complete.cases(data),]
    adjr2 <- as.numeric(summary(lm(data$pillowswe~data$todayswe))[9])
    # cvglmnet_object$cvm[which(cvglmnet_object$lambda==cvglmnet_object$lambda.1se)]
  }else{
    return(-99)
  }
}


#' @param datestr simulation date
#' @param station_data all station SWE observations
#' @export

get_best_historical_date <- function(datestr, station_data){

  today <- as.Date(datestr,'%Y%m%d')
  endhisday <- fWYBefore(today)
  station_data$Date = as.Date(as.character(station_data$dte), '%Y%m%d')
  sub_station_data <- station_data[station_data$Date <= endhisday,]
  sub_station_data <- sub_station_data[sub_station_data$Date > as.Date('19841001','%Y%m%d'),]


  snoteltoday <-
    station_data %>%
    filter(!is.na(Longitude),!is.na(Latitude)) %>%
    dplyr::filter_(~Date == today)#


  merge_station_data <- merge(sub_station_data, snoteltoday,by='Site_ID')

  select_data <- cbind.data.frame(Date = merge_station_data$Date.x, pillowswe = merge_station_data$pillowswe.x,
                                  todayswe=merge_station_data$pillowswe.y)

  selectrcn_r2 <-
    select_data%>%
    group_by(Date) %>%
    nest() %>%
    mutate(
      r2=map_dbl(data,extract_r2))

  bestrdate_csv <- selectrcn_r2$Date[which.max(selectrcn_r2$r2)]

  return(bestrdate_csv)

}
