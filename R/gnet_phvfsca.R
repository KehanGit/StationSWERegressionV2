#' Fit the GLM-Net model to the data
# https://cloud.r-project.org/web/packages/glmnet/vignettes/glmnet.pdf
#' @param dF dataframe that includes x and y variables for regression
#' @param formula formula to model
#' @param cl a cluster object for parallel execution
#' @param model compare glm and glmnet - for paper review
#' @export
#'
#'
gnet_phvfsca=function(dF,formula,cl=NULL, model = 'glmnet'){
	if(is.null(cl)) yesParallel=FALSE else yesParallel=TRUE
	nfolds=floor(nrow(dF)/20)
	if(nfolds<4)
	  nfolds=4
	if(nfolds>10) 
	  nfolds=10
	myformula=as.formula(formula)
	
	if(model == 'glm'){
	  cvfit = glm(formula = myformula, data = dF) # use the default gaussian distribution
	}else{
	  # if model == 'glmnet'
	  bestalpha = pickAlpha(dF,myformula,nfolds,cl)
	  cvfit = glmnetUtils::cv.glmnet(myformula,data=dF,nfolds=nfolds,type.measure='mse',alpha=bestalpha,
	                               parallel = yesParallel,use.model.frame=TRUE)# use the default gaussian distribution
	  cvfit$alpha=bestalpha
	}

	return(cvfit)
}
