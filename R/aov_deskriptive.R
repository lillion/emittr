#' Deskriptive Statistik pro Gruppe einer Anova
#'
#' @param lm 
#'
#' @return Deskriptive Statistik pro Gruppe
#' @export 
#'
#' @examples
lm_deskriptiv <- function(lm){
  plyr::ddply(erglm$model, formula(paste("~",paste(names(erglm$model)[-1],collapse = "+"))) , function(x) {
  N = length(x[[1]])
  Mean = mean(x[[1]])
  SD = sd(x[[1]])
  Var = var(x[[1]])
  return(c(N = N, Mean = Mean, SD = SD, Var = Var))
})
}