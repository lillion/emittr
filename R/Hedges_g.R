#' Hedges g
#'
#' @param xt1 
#' @param xt2 
#'
#' @return Ergebnis
#' @export
#'
#' @examples
#' cat("hedges")
hedgesG <- function(xt1, xt2){
  diff <- xt1-xt2
  mdiff <- mean(diff)
  sddiff <- sd(dif)
  vdiff <- var(diff)
  ndiff <- length(diff)
  stderr <- sqrt(vdiff/ndiff)
  tstat <- mdiff/stderr
#   if (scenario == "one.sample") {
#     x <- x[!is.na(x)]
#     d <- abs(mean(x) - mu)/sd(x)
#     return(d)
  df <- ndiff - 1
  pval <- pval <- pt(tstat, df)
  g <- mdiff/sddiff
    rval <- list(statistic = tstat, parameter = df, p.value = pval, 
                 g)
  # class(rval) <- "htest"
  return(rval)
}