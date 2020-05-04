#' schneller Scatterplot
#'
#' @param df 
#' @param xvar 
#' @param yvar 
#'
#' @return
#' @export
#'
#' @examples
myscatter <- function(df, xvar,yvar){
  # if(is.character(xvar)) xvar <- sym(xvar)
  # if(is.character(yvar)) yvar <- sym(yvar)
  xvar <- enexpr(xvar)
  yvar <- enexpr(yvar)
  
  ggplot(df,aes(!!xvar, !!yvar))+geom_point()+stat_smooth(method="lm")
}



#' schnelle deskriptive Statistik mit se und CI
#'
#' @param df 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples
mysummarise <- function(df, var){
  var <- enexpr(var)
  summarise(df, m=mean(!!var),sd=sd(!!var),se=sd(!!var)/sqrt(n()),ci_lower=m-se*1.96,ci_upper=m+se*1.96)
  
}