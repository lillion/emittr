#' @title linear regression similar to SPSS
#'
#' @description <full description>
#' @param fit Objekt eines linearen Modells
#' @export
#' @keywords lm, regression, spss
#' @return dataframe with lm output similar to SPSS
#' @examples 
#' library(car)
#' l <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
#' lm_output_spss(l)
#' \dontrun{
#' lapply(ll, lm_output_spss) # get outputs from several lm objects
#'}
lm_output_spss <- function (fit) {
	if(!inherits(fit,"lm")) stop(substitute(fit), " is not an lm object")
	require(car)
	scaled <- data.frame(scale(fit$model))
	lmz <- lm(formula(fit),data=scaled)
	results <- cbind(summary(fit)$coefficients,beta=coef(lmz),rbind(c(NA,NA,NA),cor_lm_d(fit)))[c(1,2,5,3,4)]
	ifelse(ncol(fit$model)==2, return(cbind(results,rbind(c(NA,NA,NA),cor_lm_d(fit)))), return(cbind(results,rbind(c(NA,NA,NA),cor_lm_d(fit)),Tolerance=c(NA,1/vif(fit)), VIF=c(NA,vif(fit)),
	Problem=c("", ifelse(sqrt(vif(fit))> 2,"yes","no")))))
}