#' @title Ergebnis einer linearen Regression ähnlich SPSS
#'
#' @description <full description>
#' @param fit Objekt eines linearen Modells
#' @param plot sollen Histogramm der Residuen, qq plot und Plot dervorhergesagten Werte/Fehler erstellt werden
#' @export
#' @keywords lm, regression, spss
#' @return dataframe des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}}
#' @examples
#' library(car)
#' fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
#' lm_output_spss_wrapper_d(fit)
lm_output_spss_wrapper_d <- function (fit, Rsquare=TRUE, coefficients=TRUE, collinearity=TRUE,plot=TRUE) {
  if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
  #lm_hier_model_sum(fit)
  results=vector("list", 0)
  if (Rsquare) results[["Model"]]=rr(fit)
  if (coefficients) results[["Coefficients"]]=lm_coef_spss_d(fit)
  if (collinearity) results[["Collinearity"]]=lillionscage:::lm_coll(fit,add.intercept=F)
  if (plot) {
    op <- par (mfrow=c(2,2))
    zresid <- scale(resid(fit))
    hist(zresid, breaks=24);
    plot(predict(fit), resid(fit)); abline(0,0)
    qqnorm(zresid); abline(0,1)
    par(op)
  }
results  
}