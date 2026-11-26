
#' @param simdate
#' @export
#' 
#' 


fdate2str <- function(simdate){
  yr=strftime(simdate,'%Y')
  doy=strftime(simdate,'%j')
  mth=strftime(simdate,'%m')
  dy=strftime(simdate,'%d')
  datestr=paste0(yr,mth,dy)
  return(datestr)
}