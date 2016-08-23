#' F-Max Test für ANOVA
#'
#' @description 
#' \itemize{
#' \item Sind die Gruppen-Stichproben gleich groß bzw. ungleich groß bis zu einer Ratio von 4:1 (größte Stichprobe 4 mal größer als die kleinste Stichprobe), sollte der Fmax-Wert kleiner als 10 sein!
#' \item Bei ungleich großen Stichproben bis zu einer Ratio von 9:1 sollte der Fmax-Wert kleiner als 3 sein.
#' \item Wenn der Fmax-Test über den Grenzwerten liegt, dann erfolgt eine Reduktion des α-Niveaus:
#' \item Fmax > 10 bei Ratio 4:1  α < .025
#' \item Fmax > 3   bei Ratio 9:1  α < .025
#' }
#' @param erglm lm Objekt
#' @param ... 
#'
#' @return Fmax, N-Ratio
#' @export
#'
#' @examples
#' fmaxtest(erglm)
fmaxtest <- function(erglm, deskriptive = TRUE){
zusfass <- plyr::ddply(erglm$model, formula(paste("~",paste(names(erglm$model)[-1],collapse = "+"))) , function(x) {
                        N = length(x[[1]])
                        Mean = mean(x[[1]])
                        SD = sd(x[[1]])
                        Var = var(x[[1]])
                        return(c(N = N, Mean = Mean, SD = SD, Var = Var))
                      })
mini <- min(zusfass$Var)
max <- max(zusfass$Var)
Fmax <- max/mini
ratio <- max(zusfass$N)/min(zusfass$N)
cat("\n------------------------------------------\n")
cat("\n                 F-Max Test               \n")
cat("\n__________________________________________\n")
if (deskriptive) print(zusfass)
cat("\n")
return(list(Fmax=Fmax,NRatio=ratio))
  
}  

