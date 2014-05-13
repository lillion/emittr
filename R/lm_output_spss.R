#' @title linear regression similar to SPSS
#'
#' @description <full description>
#' @param fit Objekt eines linearen Modells
#' @export
#' @keywords lm, regression, spss
#' @return dataframe with lm output similar to SPSS
#' @examples \dontrun{
#'
#'}
lm_output_spss <- function (fit) {
	if(class(fit)!="lm") stop(substitute(fit), " is not an lm object")
	require(car)
	scaled <- data.frame(scale(fit$model))
	lmz <- lm(formula(fit),data=scaled)
	cbind(summary(fit)$coefficients,beta=coef(lmz),rbind(c(NA,NA,NA),cor_lm(fit)),Tolerance=c(NA,1/vif(fit)),VIF=c(NA,vif(fit)))[c(1,2,5,3,4,6:10)]

}