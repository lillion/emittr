# 
#' @title  semipartial, partial und Korrelationen nullter Ordnung eines Regressionsmodell
#'
#' @description berechnet semipartial, partial und Korrelationen nullter Ordnung der Prädiktoren einer linearen Regression mit dem Kriterium
#' @param fit ein Objekt einer \code{\link{lm}} Berechnung
#' @export
#' @keywords "linear model" lm regression korrelation
#' @seealso \code{\link{lm},\link{cor}}
#' @return Datensatz (dataframe) von r, r.,r..
#' @examples
#' library(car)
#' fit <- lm(mpg ~ disp + hp + wt + drat, data=mtcars) 
#' cor_lm_d(fit)
cor_lm_d <- function(fit, stellen=2)  {
  if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
  dv <- names(fit$model)[1]
  dv_data <- fit$model[, dv]
  ivs <- names(fit$model)[-1]
  iv_data <- fit$model[, ivs]
  if (length(ivs)==1) return (data.frame("nullter Ordnung"=cor(iv_data,dv_data),row.names=ivs)) # check if just one predictor
  x <- fit$model
  x_omit <- lapply(ivs, function(X) x[, c(dv, setdiff(ivs, X))])
  names(x_omit) <- ivs
  lapply(x_omit, head)
  fits_omit <- lapply(x_omit, function(X) lm(as.formula(paste(dv, "~ .")), 
                                             data = X))
  resid_omit <- sapply(fits_omit, resid)
  iv_omit <- lapply(ivs, function(X) lm(as.formula(paste(X, "~ .")), data = iv_data))
  resid_iv_omit <- sapply(iv_omit, resid)
  
  results <- sapply(seq(ivs), function(i) c(nullterOrdnung = cor(iv_data[, i], dv_data), 
                                            partial = cor(resid_iv_omit[, i], resid_omit[, i]), 
                                            semipartial = cor(resid_iv_omit[,i], dv_data)))
  results <- data.frame(t(results))
  if(is.numeric(stellen)) results <- round.df(results,stellen)
  
  rownames(results) <- ivs
  names(results)[1] <- "nullter Ordnung"
  results
}