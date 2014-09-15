#' @title Ergebnis einer linearen Regression ähnlich SPSS
#'
#' @description Dreigeteiltes Ergebnis einer linearen Regression ähnlich der Ausgabe in SPSS.  Neben einem Modelübersicht mit Varianzaufklärung werden die Koeffizienten aufgeführt und eine Kollinearitätsdiagnose angeboten.
#' @param fit Objekt eines linearen Modells
#' @param Rsquare (logisch) soll das R^2 berechnet werden
#' @param coefficients (logisch) sollen die Koeffizienten angezeigt werden
#' @param collinearity (logisch) soll eine Kollinearitätsdiagnose durchgeführt werden
#' @param plot sollen Histogramm der Residuen, qq plot und Plot der vorhergesagten Werte/Fehler erstellt werden
#' @param runden sollen die Ergebnisse auf x Stellen gerundet werden. Sinnvoll sind 2 oder 3
#' @param sterne (logisch) sollen Signifikanzsterne angegeben werden
#' @export
#' @keywords lm, regression, spss
#' @return Liste mit 3 Elementen (dataframe) des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}}
#' @examples
#' library(car)
#' fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
#' lm_output_spss_wrapper_d(fit, runden=2)
#' 
#' \dontrun{
#' # um "schöne" Tabellen zu bekommen
#' kable(lm_output_spss_wrapper_d(l))
#' lapply(ll, function(x) kable(lm_output_spss_wrapper_d(x)))
#' }
lm_output_spss_wrapper_d <- function (fit, Rsquare=TRUE, coefficients=TRUE, collinearity=TRUE,plot=TRUE, runden=NA, sterne=T) {
  if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
  #lm_hier_model_sum(fit)
  results=vector("list", 0)
  if (Rsquare) results[["Modell"]]=rr(fit)
  if (coefficients) results[["Koeffizienten"]]=lm_coef_spss_d(fit,sterne=sterne)
  if (collinearity) results[["Kollinearität"]]=lillionscage:::lm_coll(fit,add.intercept=F)
  if (plot) {
    op <- par (mfrow=c(2,2))
    zresid <- scale(resid(fit))
    hist(zresid, breaks=24);
    plot(predict(fit), resid(fit)); abline(0,0)
    qqnorm(zresid); abline(0,1)
    par(op)
  }
  if(is.numeric(runden)) return(sapply(results, umittr:::round_df, digits=runden))
results  
}