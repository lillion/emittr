#' @title Lineare Regression ähnlich der aus SPSS
#'
#' @description <full description>
#' @param fit Objekt eines linearen Modells
#' @export
#' @keywords lm, regression, spss
#' @return dataframe des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}}
#' @examples \dontrun{
#'
#'}
lm_output_spss_d <- function (fit) {
	if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
	require(car)
	scaled <- data.frame(scale(fit$model))
	lmz <- lm(formula(fit),data=scaled)
	cbind(summary(fit)$coefficients,beta=coef(lmz),rbind(c(NA,NA,NA),cor_lm_d(fit)),Tolerance=c(NA,1/vif(fit)),VIF=c(NA,vif(fit)))[c(1,2,5,3,4,6:10)]

}