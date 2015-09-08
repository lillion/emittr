#' @title \code{Ü}bersicht der Ergebnisse einer Hierarchischen Regression
#'
#' @description Diese Funktion führt eine hierarchische Regression mit mehreren Blöcken durch und bietet sowohl eine Modellübersicht mit den Veränderungen in R² und deren Signifikanz, als auch eine Übersicht der Koeffizienten pro Modell
#' @param formula die Formel des vollständigen, d.h. alle Blöcke umfassenden Modells
#' @param blocks ein Vektor mit der Prädiktorenanzahl pro Block im Format c(Anzahl1,Anzahl2,…)
#' @param stellen auf wie viele Stellen soll der Output gerundet werden? Voreingestellt 2.
#' @param ... Optionen, die an \code{\link{lm}} weitergegeben werden sollten, z.B. data=
#' @export
#' @keywords lm regression
#' @seealso \code{\link{lm}}, \code{\link{lm_hierarch_d}}
#' @return list mit den Ergebnissen
#' @examples
#' library(car)
#' lm_hierarch_summary_d(formula = mpg ~ disp + hp + wt + drat, 
#' blocks = c(1,1,1,1), # 4 Blöcke mit je einem zusätzlichen Prädiktor
#' data=mtcars)
lm_hierarch_summary_d <- function (formula, blocks, stellen=2, ...) {
  cat("\nHIERARCHISCHE REGRESSION\n\n")
  ll <- lm_hierarch_d(formula=formula, blocks=blocks, summary=FALSE, ...)
  r1 <- t(sapply(ll, function(x) {temp=summary(x); f=temp$fstatistic;c(R=sqrt(temp$r.squared),"R\U00B2"=temp$r.squared, p = unname(pf(f[1],f[2],f[3],lower.tail=FALSE)),"adj R\U00B2"=temp$adj.r.squared)}))
  #print(r1)
  
  ano <- paste(sapply(1:length(ll), function(x) paste0("ll[[",x,"]]")), collapse=",")
  #print(parse(text=paste("anova(",ano,")")))
  r2 <- eval(parse(text=paste("anova(",ano,")")))
  #r2 <- eval(parse(text=eval(substitute(paste("anova(",ano,")")))))
  
  r2 <- r2[c(5,3,1,6)]
  f <- summary(ll[[1]])$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=FALSE)
  r2[1,] <- c(summary(ll[[1]])$fstatistic,p)
  R2chg<- c(r1[1,2],diff(r1[,2]))
  result <- cbind(r1,R2chg,r2)
  names(result) <- c("R", "R²", "p", "adj R²",  "R²chg",    "F chg", "Df", "Res.Df", "p chg")
  print(round(result,stellen))
  cat("\n \n")
  sapply(ll, function(x) {round.df(lm_coef_spss_d(x, stellen=stellen),stellen)})
  
}