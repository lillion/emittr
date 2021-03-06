#' @title Ergebnis einer linearen Regression aehnlich SPSS
#'
#' @description Dreigeteiltes Ergebnis einer linearen Regression ähnlich der Ausgabe in SPSS.  Neben einem Modelübersicht mit Varianzaufklärung werden die Koeffizienten aufgeführt und eine Kollinearitätsdiagnose angeboten.
#' @param fit Objekt eines linearen Modells
#' @param Rsquare (logisch) soll das R^2 berechnet werden
#' @param coefficients (logisch) sollen die Koeffizienten angezeigt werden
#' @param collinearity (logisch) soll eine Kollinearitätsdiagnose durchgeführt werden
#' @param plot (logisch) sollen Histogramm der Residuen, qq plot und Plot der vorhergesagten Werte/Fehler erstellt werden
#' @param runden (nummerisch) Anzahl der Stellen, auf die die Ergebnisse gerundet werden. Sinnvoll sind 2 oder 3
#' @param sterne (logisch) sollen Signifikanzsterne angegeben werden
#' @param fix (logisch) sollen p Werte gerundet 0 als <.001 angezeigt werden
#' @export
#' @keywords lm regression spss
#' @return Liste mit 3 Elementen (dataframe) des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}} \code{\link{lm_coef_spss_d}}
#' @examples
#' library(car)
#' fit <- lm(mpg ~ disp + hp + wt + drat, data=mtcars) 
#' lm_output_spss_wrapper_d(fit, runden=2)
#' 
#' \dontrun{
#' # um "schöne" Tabellen zu bekommen
#' kable(lm_output_spss_wrapper_d(l))
#' lapply(ll, function(x) kable(lm_output_spss_wrapper_d(x)))
#' }
lm_output_spss_wrapper_d <- function (fit, Rsquare=TRUE, coefficients=TRUE, collinearity=TRUE,plot=TRUE, runden=3, sterne=TRUE, fix=TRUE) {
  if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
  #lm_hier_model_sum(fit)
  results=vector("list", 0)
  if (Rsquare) results[["Modell"]]=rr(fit)
  if (coefficients) results[["Koeffizienten"]]=lm_coef_spss_d(fit,sterne=sterne, fix=fix)
  if (collinearity) results[["Kollinearität"]]=lm_coll(fit,add.intercept=FALSE)
  if (plot) {
    # op <- par (mfrow=c(2,2))
    zresid <- data.frame(zResiduen=scale(resid(fit)))
    # hist(zresid, breaks=24);
    # plot(predict(fit), resid(fit)); abline(0,0)
    # qqnorm(zresid); abline(0,1)
    # par(op)
    library(ggplot2)
    library(gridExtra)
    p1 <- ggplot(zresid,aes(zResiduen))+geom_histogram(bins=dim(zresid)[1]/1.1)+geom_vline(xintercept=0)+theme_bw(,"Helvetica Light")+ggtitle("Verteilung der Residuen")
    predictdata <- data.frame(predicted=predict(fit), residuals=resid(fit))
    p2 <- ggplot(predictdata,aes(predicted,residuals))+geom_point(size=.9)+geom_smooth()+theme_bw(,"Helvetica Light")+ggtitle("Skedastizität")
    p3 <- umittr:::qqplot.data(zresid$zResiduen)+ggtitle("QQ-Plot der Residuen")
    p1;p2;p3
    lay=rbind(c(1,1),c(2,3))
    grid.arrange(p2,p1,p3,layout_matrix=lay)
  }
  if(is.numeric(runden)) return(sapply(results, round.df, digits=runden))
results  
}