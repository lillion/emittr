#' @title Predictive R-squared
#' @author Thomas Hopper
#' @description returns the predictive r-squared using 
#'              the PRESS statistic.
#' @param linear.model A linear regression model (class 'lm'). Required.
#'
lm_pred_r_squared <- function(l) {
  if(!inherits(l,"lm")) stop(substitute(l), " is not an lm object")
  lm.anova <- anova(l)
  tss <- sum(lm.anova$'Sum Sq')
  pr <- residuals(l)/(1-lm.influence(l)$hat)
  PRESS <- sum(pr^2)
  pred.r.squared <- 1-PRESS/(tss)
  return(pred.r.squared)
}


