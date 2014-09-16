#' @title Linear Regression Summary similar to SPSS
#'
#' @description <full description>
#' @param fit Objekt eines linearen Modells
#' @param plot should histogram of residuals, qq plot and predicted/residual value plot be shown
#' @export
#' @keywords lm, regression, spss
#' @return dataframe des Ergebnisses einer Regression ähnlich der in SPSS
#' @seealso \code{\link{lm}}
#' @examples 
#' library(car)
#' fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
#' lm_output_spss_wrapper(fit)
#'
lm_output_spss_wrapper <- function (fit, Rsquare=TRUE, coefficients=TRUE, collinearity=TRUE,plot=TRUE) {
  if(class(fit)!="lm") stop(substitute(fit), " is not a LM object")
  #lm_hier_model_sum(fit)
  results=vector("list", 0)
  if (Rsquare) results[["Model"]]=rr(fit)
  if (coefficients) results[["Coefficients"]]=lm_coef_spss(fit)
  if (collinearity) results[["Collinearity"]]=lm_coll(fit,add.intercept=F)
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