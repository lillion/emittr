#' @title Hierarchical Regression model summary
#'
#' @description given a hierarchical regression with several blocks, gives an overview over changes in R^2 and significance
#' @param ll a list containing regular lm objects
#' @export
#' @keywords lm, regression
#' @seealso \code{\link{lm}}, \code{\link{lm_hierarch}}
#' @return dataframe of summaries
#' @examples \dontrun{
#'
#'}
#' library(car)
#' ll <- lm_hierarch(mpg~disp+hp+wt+drat, c(1,1,1,1), data=mtcars,summary=FALSE)
#' lm_hier_model_sum(ll)
#' #rm(ll)
lm_hier_model_sum <- function (ll) {
  if (class(ll[[1]])!="lm") stop("Please don't use a summary lm.object!")
  r1 <- t(sapply(ll, function(x) {c(R=sqrt(summary(x)$r.squared),rsq=summary(x)$r.squared,adjrsq=summary(x)$adj.r.squared)}))
  #print(r1)
  
  ano <- paste(sapply(1:length(ll), function(x) paste0("ll[[",x,"]]")), collapse=",")
  #print(parse(text=paste("anova(",ano,")")))
  r2 <- eval(parse(text=paste("anova(",ano,")")))
  #r2 <- eval(parse(text=eval(substitute(paste("anova(",ano,")")))))
  
  r2 <- r2[c(5,3,1,6)]
  f <- summary(ll[[1]])$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  r2[1,] <- c(summary(ll[[1]])$fstatistic,p)
  rch <- c(r1[1,2],diff(r1[,2]))
  cbind(r1,rch,r2)
}