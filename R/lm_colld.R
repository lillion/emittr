#' @title multi collinearity diagnosis inside a linear regression
#'
#' @description multi collinearity diagnosis
#' @param l lm object
#' @param ... 
#' @keywords internal
#' @return data.frame colldiag
#' @importFrom perturb colldiag
#' @examples \dontrun{
#'
#'}
lm_coll <- function(l,...){
  #require(perturb); 
  #results[["Collinearity"]]=data.frame(colldiag(l,...)$condindx,colldiag(l,...)$pi)
  data.frame(colldiag(l,...)$condindx,colldiag(l,...)$pi)
}