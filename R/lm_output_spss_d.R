#' @title Koeffizienten einer Lineare Regression dargestellt aehnlich SPSS
#'
#' @description Diese Funktion liefert die Tabelle der Regressionskoeffizienten (beta) ähnlich der Darstellung in SPSS
#' @param fit Objekt eines linearen Modells
#' @param sterne (logisch) sollen Signifikanzsterne (\code{\link{symnum}}) gedruckt werden
#' @export
#' @keywords lm regression spss
#' @return dataframe des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}}
#' @examples 
#' data(crime_data)
#' fit <- lm(crime ~ pctwhite + pcths + pctmetro + single + poverty,data=crime_data)
#' lm_coef_spss_d(fit)
#' \dontrun{
#' lapply(ll, lm_output_spss) # get outputs from several lm objects
#'}
lm_coef_spss_d <- function (fit, sterne=TRUE, stellen=2) {
	if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
	suppressPackageStartupMessages(require(car))
	scaled <- data.frame(scale(fit$model))
	lmz <- lm(formula(fit),data=scaled)
	results <- cbind(summary(fit)$coefficients,beta=coef(lmz),rbind(c(NA,NA,NA),cor_lm_d(fit)))[c(1,2,5,3,4)]
  results <- round.df(results,stellen)
  names(results) <- c("B","Std Error","\u03B2","t","p")
  if (sterne) {results <- cbind(results,data.frame(st=as.character(symnum(summary(fit)$coefficients[,4],corr=FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1,1),symbols = c("***","**","*","+"," "))))); names(results) <- c("B","Std Error","\U1D6FD","t","p"," ")}
	ifelse(ncol(fit$model)==2, return(cbind(results,rbind(c(NA,NA,NA),cor_lm_d(fit)))), return(cbind(results,rbind(c(NA,NA,NA),cor_lm_d(fit)),Tolerance=c(NA,1/vif(fit)),VIF=c(NA,vif(fit)),Problem=c(NA,ifelse(sqrt(vif(fit))> 2,"ja","nein")))))

}