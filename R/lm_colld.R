#' @title <brief desc>
#'
#' @description <full description>
#' @param l 
#' @param ... 
#' @keywords internal
#' @return data.frame colldiag
#' @importFrom perturb colldiag
#' @examples \dontrun{
#'
#'}
lm_coll <- function(l,...){
  #require(perturb); 
  results[["Collinearity"]]=data.frame(colldiag(l,...)$condindx,colldiag(l,...)$pi)
}