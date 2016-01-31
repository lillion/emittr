#' spss einflesen
#' wrapper für die foreign Funtion
#' @param file Pfad zur Datei 
#'
#' @return data.frame
#' @export
#'
spss <- function(file){
  require(foreign)
  read.spss(file,use.value.labels = F,to.data.frame = T)
}



#' T/SW Werte in Prozenz
#' wandelt einzelne oder mehrere T- bzw. S(W)-Werte in Prozent um
#' @param twert 
#' @param SW logisch wenn TRUE, dann Standardwerte (MW 100, SD 10)
#' @return Prozentwerte
#' @export
#'
#' @examples
#' tprozent(20:80)
tprozent <- function(twert, SW=FALSE){
  if(SW) twert <- twert-50
  z <- (twert-50)/10
  prozent <- pnorm(z)*100
  cat("T Wert:",twert, "\nz Wert:",z,"\nProzent:", prozent)
}

#' Odds zu Wahrscheinlichkeiten umwandeln
#'
#' @param odds einzeln oder als Vektor oder als glm Objekt
#'
#' @return p Wert
#' @export
#'
#' @examples
#' odds2p(c(1,2,.5))
odds2p <- function(odds) {
  if(odds$family[1]=="binomial") {
    odds <- exp(coef(odds))
      }
  p <- odds/(odds + 1)    
  p[is.nan(p)] <- 1
  p}



#' Wahrscheinlichkeiten (p) in Odds umwandeln
#'
#' @param p 
#'
#' @return Odds
#' @export
#'
#' @examples
#' p2odds(c(.10,.50,.90))
p2odds <- function(p) {p/(1 - p)}




