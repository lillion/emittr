#' Kontrastanalyse für abhängige Stichproben
#'
#' @param daten 
#' @param kontrast 
#'
#' @return data.frame
#' @export
#'
#' @examples
kontrastAbh <- function(daten, kontrast){
  if(dim(as.matrix(daten))[2]!=dim(matrix(kontrast))[1]) return(print("Kontrastanzahl passt nicht zu den Daten"))
  kon <- as.matrix(daten)%*%matrix(kontrast)
  L <- sum(kon)/dim(kon)[1]
  squad <- sum(sapply(kon, function(x) (x-L)^2))/(dim(kon)[1]-1)
  twert <- L/sqrt(squad/dim(kon)[1])
  df=(dim(kon)[1]-1)
  pt(twert,df =df,lower.tail = F )
  Hedges_G=L/sqrt(squad)
  # print(data.frame(twert=twert,df=df,p=pt(twert,df =df,lower.tail = F ),Var=squad,L=L))
  cat("Kontrastanalyse für abhängige Daten\nKontrast:", paste0(names(daten),": ",kontrast),"\n\n")

  return(data.frame(twert=twert,df=df,F=twert^2,p=pt(twert,df =df,lower.tail = F ),Var=squad,L=L,Hedges_G=Hedges_G))
}

# 
# kontrastAbh(datenMW[1:3],c(-1,0,1))
# kontrastAbh(datenMW[1:3],c(.5,-1,.5))
# 15.03^2
# 23.65^2
# kontrastAbh(datenMW[1:3],c(1,-1,0))
# 16.09^2
# kontrastAbh(datenMW[1:3],c(0,-1,1))
# 23.62^2
# kontrastAbh(datenMW[1:3],c(-1,0,1))
# 23.62^2
# kontrastAbh(testdaten,c(1.25,.25,-.75,-.75))
# 
