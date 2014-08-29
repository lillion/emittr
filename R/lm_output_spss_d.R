#' @title Koeffizienten einer Lineare Regression dargestellt aehnlich SPSS
#'
#' @description Diese Funktion liefert die Tabelle der Regressionskoeffizienten (β) ähnlich der Darstellung in SPSS
#' @param fit Objekt eines linearen Modells
#' @export
#' @keywords lm, regression, spss
#' @return dataframe des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}}
#' @examples \dontrun{
#'
#'}
lm_coef_spss_d <- function (fit) {
	if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
	require(car)
	scaled <- data.frame(scale(fit$model))
	lmz <- lm(formula(fit),data=scaled)
	results <- cbind(summary(fit)$coefficients,beta=coef(lmz),rbind(c(NA,NA,NA),cor_lm_d(fit)))[c(1,2,5,3,4)]
	ifelse(ncol(fit$model)==2, return(cbind(results,rbind(c(NA,NA,NA),cor_lm_d(fit)))), return(cbind(results,rbind(c(NA,NA,NA),cor_lm_d(fit)),Tolerance=c(NA,1/vif(fit)),VIF=c(NA,vif(fit)),Problem=c(NA,sqrt(vif(fit)> 2)))))

}