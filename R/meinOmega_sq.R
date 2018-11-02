#' Omega Quadrat einer Anova ausgeben
#'
#' @param Anova3mod 
#'
#' @description Mit dieser Funktion kann die Effektstärke Omega2 einer ANOVA berechnet werden
#' @return omega2
#' 
#' @export
#' @examples
omegasq <- function(Anova3mod){
  temp <- Anova3mod
  if(!"anova" %in% class(temp)) temp <- car::Anova(Anova3mod,type=3)
  rnames <- rownames(temp)
  n <- length(temp[["Sum Sq"]])
  MSSn <- mutate(temp,MSS=temp$'Sum Sq'/Df)
  MSSn <- MSSn[-1,]
  
  TotalSum <- sum(MSSn$`Sum Sq`)
  ResidualMSS <- MSSn[n-1,"MSS"]
  MSSFactors <- MSSn[-(n-1),]
  omegasq <- MSSFactors$Df*(MSSFactors$MSS-ResidualMSS)/(TotalSum+ResidualMSS)
  factor <- rnames[-c(1,n)]
  return(data.frame(factor,omegasq))
}


#' Eta Quadrat und partielles Eta Quadrat einer Anova ausgeben
#'
#' @param Anova3mod 
#'
#' @description Mit dieser Funktion kann die Effektstärke η²und pEta2 einer ANOVA berechnet werden
#' @return η², peta2
#' 
#' @export
#' @examples
etasq <- function(Anova3mod){
  temp <- Anova3mod
  if(!"anova" %in% class(temp)) temp <- car::Anova(Anova3mod,type=3)
  rnames <- rownames(temp)
  n <- length(temp[["Sum Sq"]])
  MSSn <- mutate(temp,MSS=temp$'Sum Sq'/Df)
  MSSn <- MSSn[-1,]
  
  TotalSum <- sum(MSSn$`Sum Sq`)
  ResidualSumSS <- MSSn[n-1,"Sum Sq"]
  MSSFactors <- MSSn[-(n-1),]
  etasq <- MSSFactors$Df*MSSFactors$MSS/(TotalSum)
  etasqp <- MSSFactors$Df*MSSFactors$MSS/(MSSFactors$Df*MSSFactors$MSS+ResidualSumSS)
  factor <- rnames[-c(1,n)]
  return(data.frame(factor,etasq,etasqp))
}

