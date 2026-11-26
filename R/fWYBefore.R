

#' @param simdate
#' @export
#' 
#' 



fWYBefore <- function(simdate){
  
  yr=strftime(simdate,'%Y')
  doy=strftime(simdate,'%j')
  mth=strftime(simdate,'%m')
  dy=strftime(simdate,'%d')
  datestr=paste0(yr,mth,dy)
  
  if(mth < 10){
    backdate <- as.Date(paste0(as.character(as.numeric(yr)-1),'0930'),'%Y%m%d')
  }else{
    backdate <- as.Date(paste0(as.character(as.numeric(yr)),'0930'),'%Y%m%d')
  }
  return(backdate)
}
