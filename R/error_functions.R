#' mean absolute error as percent of mean obs
library(hydroGOF)
#' @param model model object
#' @param data dataframe to predict with modelobj
#' @export

# new updates on 20220731 Kehan Yang
mypctmae2=function(model,data) {
  yvar=all.vars(formula(model)[[2]])
  yhat=predict(model, data)
  yobs=as.data.frame(data)[[yvar]]
  df <- cbind.data.frame(yhat,yobs)
  names(df) <- c('sim','obs')
  # hydroGOF::pbias(df$sim,df$obs, na.rm=T)
  return(hydroGOF::mae(df$sim,df$obs)/mean(yobs,na.rm=T)*100)
  
  
}


#' root mean squared error
#' @param yobs observed
#' @param yhat predicted
#' @export
rootmse = function(yobs,yhat){
  return(sqrt(mean((yobs-yhat)^2,na.rm=T)))
}

#' mean absolute error as percent of mean obs
library(hydroGOF)
#' @param model model object
#' @param data dataframe to predict with modelobj
#' @export
#'
mypctmae=function(model,data) {
  yvar=all.vars(formula(model)[[2]])
  return(hydroGOF::mae(model,data)/mean(as.data.frame(data)[[yvar]],na.rm=T)*100)
}




#' @param model model object
#' @param data dataframe to predict with modelobj
#' @details there are two rsquare functions because I clipped them from different pieces of code. They take different types of inputs but perform the same calculation.
#' @export

myr2=function(model,data){
  yvar=all.vars(formula(model)[[2]])
  yhat=predict(model, data)
  # print(yhat)
  yobs=as.data.frame(data)[[yvar]]
  # print(yobs)
  return(stats::cor(yhat, yobs, use='complete.obs')^2)
}





#' percentage vias
#'
#' @param model
#' @param data
#' @export
#'
mypbias=function(model,data) {
  yvar=all.vars(formula(model)[[2]])
  yhat=predict(model, data)
  yobs=as.data.frame(data)[[yvar]]
  df <- cbind.data.frame(yhat,yobs)
  names(df) <- c('sim','obs')
  return(hydroGOF::pbias(df$sim,df$obs, na.rm=T))
  
}
