#' Title
#'
#' @param formula 
#' @param data 
#'
#' @return aov
#' @export
#'
#' 
#' 
Anova3 <- function(formula, data, Median=TRUE, posthoc=TRUE){
  library(car)
  options(contrasts = c("contr.sum","contr.poly"))
  erglm <- lm(formula, data=data)
  print(psych::describeBy(erglm$model,attr(erglm$terms, "term.labels")[1:length(erglm$model)-1],range=F))
  #with(erglm$model,get(names(erglm$model)[1]))
  cat("\n__________________________\n\nLevene Test\n\n")
  s <- ifelse(Median, "median","mean")
  print(car::leveneTest(formula, data=data,s))
  print(fmaxtest(erglm))
  cat("\n__________________________\n\nANOVA Tabelle\n\n")
  # ergaov <- Anova(erglm,type=3)
  # print(ergaov)
  print(ergaov <- heplots::etasq(erglm,anova=T,type=3))
  if(posthoc) print(agricolae::HSD.test(erglm, attr(erglm$terms, "term.labels")[1:length(erglm$model)-1], group=T,console = T))
  invisible(ergaov)
}



