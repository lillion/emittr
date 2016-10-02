#' Kontrastvergleich für abhängige Stichproben
#'
#' @param daten 
#' @param kontrast 
#'
#' @return data.frame
#' @export
#'
#' @examples
kontrastVergleichAbh <- function(daten, kontrast1, kontrast2){
  if(dim(as.matrix(daten))[2]!=dim(matrix(kontrast1))[1]) return(print("Kontrastanzahl passt nicht zu den Daten"))
  s1 <- sqrt(sum(kontrast1^2)/length(kontrast1))
  s2 <- sqrt(sum(kontrast2^2)/length(kontrast2))
  kontrast1 <- kontrast1/s1
  kontrast2 <- kontrast2/s2
  kon1 <- as.matrix(daten)%*%matrix(kontrast1)
  kon2 <- as.matrix(daten)%*%matrix(kontrast2)
  kon <- kon2-kon1
  L <- mean(kon)
  squad <- var(kon)#sum((kon-L)^2)
  twert <- L/sqrt(squad/dim(kon)[1])
  df=(dim(kon)[1]-1)
  p <- pt(twert,df =df,lower.tail = F )
  Hedges_G=twert/sqrt(dim(kon)[1])
  # cat("Kontrastanalyse für abhängige Daten\nKontrast:", paste0(names(daten),": ",kontrast),"\n\n")
  # 
  return(data.frame(twert=twert,df=df,F=twert^2,p=p,Var=squad,L=L,Hedges_G=Hedges_G))
  #return(s2)
}


# kontrastVergleichAbh(testdaten,c(1.25,.25,-.75,-.75),c(3,-1,-1,-1))
