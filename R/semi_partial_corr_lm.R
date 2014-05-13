# 
#' @title semipartial, partial and zero-order correlations for a LM
#'
#' @description <full description>
#' @param fit lm object
#' @export
#' @keywords linear model, lm, regression
#' @seealso lm() 
#' @return dataframe of r, r.,r..
#' @examples \dontrun{
#'
#'}
cor_lm <- function(fit)  {
  if(class(fit)!="lm") stop(substitute(fit), " is not an lm object")
  dv <- names(fit$model)[1]
  dv_data <- fit$model[, dv]
  ivs <- names(fit$model)[-1]
  iv_data <- fit$model[, ivs]
  if (length(ivs)==1) return (data.frame(zeroorder=cor(iv_data,dv_data),row.names=ivs)) # check if just one predictor
  x <- fit$model
  x_omit <- lapply(ivs, function(X) x[, c(dv, setdiff(ivs, X))])
  names(x_omit) <- ivs
  lapply(x_omit, head)
  fits_omit <- lapply(x_omit, function(X) lm(as.formula(paste(dv, "~ .")), 
                                             data = X))
  resid_omit <- sapply(fits_omit, resid)
  iv_omit <- lapply(ivs, function(X) lm(as.formula(paste(X, "~ .")), data = iv_data))
  resid_iv_omit <- sapply(iv_omit, resid)
  
  results <- sapply(seq(ivs), function(i) c(zeroorder = cor(iv_data[, i], dv_data), 
                                            partial = cor(resid_iv_omit[, i], resid_omit[, i]), 
                                            semipartial = cor(resid_iv_omit[,i], dv_data)))
  results <- data.frame(results)
  
  names(results) <- ivs
  results <- data.frame(t(results))
  results
}