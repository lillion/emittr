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