#' Berechnung von Konfidenzintervallen
#'
#' @param wert Roh- bzw. Standardwert im Test
#' @param standardabweichung Die Standardabweichung des verwendeten Werts
#' @param reliabilitaet Reliabilität des Tests - zwischen 0 und 1
#' @param seitigkeit 1 oder 2
#' @param sicherheit Sicherheitswahrscheinlichkeit zwischen 0 und 1
#' @param mittelwert für Äquivalenzhypothese: Mittelwert der Verteilung
#' @param typ "a" Äquivalenzhypothese "r" Regressionshypothese
#'
#' @return Konfidenzintervall
#' @export
#'
#' @examples
#' KI(wert = 66,standardabweichung = 10,reliabilitaet = .98,sicherheit = .99,seitigkeit = 1,mittelwert = 50,typ = "r") # inklusive Regressionshypothese
KI <- function(wert, standardabweichung, reliabilitaet, seitigkeit=1, sicherheit=.80, mittelwert=NA, typ="a"){
  sex <- standardabweichung*sqrt(1-reliabilitaet)
  set <- standardabweichung*sqrt(reliabilitaet*(1-reliabilitaet))
  if(!(seitigkeit==1 | seitigkeit==2)) {cat("bitte 1 oder 2 angeben bei Seitigkeit!");stop()}
  z <- 1-((1-sicherheit)/seitigkeit)
  if(typ=="r" & !is.na(mittelwert)) {wertt <- reliabilitaet*wert+mittelwert*(1-reliabilitaet)}
  b <- sex*qnorm(z)
  bt <- set*qnorm(z)
  cat("\nÄquivalenz: ", c(wert-b,wert+b))
  if(exists("wertt")) cat("\nRegression: ",c(wertt-bt,wertt+bt))
}




