
#' setup and clean cocorhas inventory
#'#' @export

#' @param datestr the simdate 
#' @param Path_CCR the simdate 
#' @param cos_density the costum density  
#' @param template raster template to get the extent of model domain

#' @return 


# datestr <- '20200106'
get_ccr <- function(datestr,Path_CCR,cos_density, template){
  # EXTENT_NORTH = 41.375
  # EXTENT_EAST = -117.625
  # EXTENT_SOUTH = 35.125
  # EXTENT_WEST = -122.375
  EXTENT_NORTH = extent(template)[4]
  EXTENT_EAST = extent(template)[2]
  EXTENT_SOUTH = extent(template)[3]
  EXTENT_WEST = extent(template)[1]
  
  date <- as.Date(datestr, '%Y%m%d')
  yr=strftime(date,'%Y')
  mth=strftime(date,'%m')
  dy=strftime(date,'%d')
  datestr_url=paste0(mth,'/',dy,'/', yr)
  
  url <- paste0('http://data.cocorahs.org/export/exportreports.aspx?ReportType=Daily&dtf=1&Format=CSV&ReportDateType=reportdate&Date=',datestr_url,'&TimesInGMT=False')
  
  
  # url <- 'http://data.cocorahs.org/export/exportreports.aspx?ReportType=Daily&dtf=1&Format=CSV&State=CA&ReportDateType=reportdate&StartDate=1/1/1984&EndDate=1/23/2020&TimesInGMT=False'
  new_file=paste0(Path_CCR,'/ccr_',datestr,'.csv')
  dstatus <- download.file(url,new_file,method="auto")
  
  data <- read.csv(new_file, stringsAsFactors = F)
  
  
  df <- cbind.data.frame(Site_ID = data$StationNumber,
                         Latitude = data$Latitude,
                         Longitude = data$Longitude,
                         dte = data$ObservationDate,
                         swe = as.numeric(data$TotalSnowSWE)*0.0254,  # inch to meter
                         depth = as.numeric(data$TotalSnowDepth)*0.0254)
  
  head(df)
  
  df <- df %>%
    filter(Latitude>=EXTENT_SOUTH, Latitude<=EXTENT_NORTH, Longitude<= EXTENT_EAST, Longitude>= EXTENT_WEST)
  
  df <- df[complete.cases(df$depth),]
  print('The original cocorahs data:')
  print(df)
  if(sum(!is.na(df$depth))>0){
    if(mean(df$swe, na.rm=T) == 0){
      if(mean(df$depth, na.rm=T) > 0){
        mean_density <- cos_density # the mean snow density of fresh snow
      }
      df$swe <- df$depth * mean_density
      
      
    }else{
      dfswe <- df[complete.cases(df$swe),]
      mean_density <- mean(dfswe$swe)/mean(dfswe$depth)
      df$swe[is.na(df$swe)] <- df$depth * mean_density
    }
  }
 
  return(df)
}
