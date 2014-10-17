#' @title compute corrected r2 and omega2 for a regression
#'
#' @description compute corrected r2 and omega2 for a regression
#' as described by Stelzl (2005)
#' @param lmm 
#' @export
#' @keywords lm
#' @seealso lm
#' @return r2, omega2
#' @examples \dontrun{
#'
#'}
r2_stelzl <- function(lmm){
  r2 <- summary(lmm)$r.squared
  n <- dim(lmm$model)[1]
  p <- dim(lmm$model)[2]-1
  rkorr <- r2-((p-2)*(1-r2))/(n-p-1) - (2*(n-3)/((n-p-1)*(n-p+1))) *(1-r2)^2
  omega2 <- ((n-p-3)*rkorr^2+rkorr)/((n-2*p-2)*rkorr+p)
  return(data.frame(rkorr,omega2))
}