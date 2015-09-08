#' @title Variablen-Reihenfolge aendern
#'
#' @description für ein simples Umstellen der Variablenreihenfolge im Datensatz
#' @param variablen columns to put at the left side or right side of a dataframe
#' @param data Datensatz
#'
#' @return a dataframe with the reordered columns
#' @export
#'
#' @examples
#' data(crime_data)
#' temp <- varumstellen(c("crime","murder"), crime_data)
#' head(temp)
#' rm(temp)
varumstellen <- function(variablen, data) {
  if (!class(variablen)=="character") stop("Variablen müssen als Strings angegeben werden!\nz.B. c(\"Alter\",\"Geschlecht\")")
  return( data[, c(variablen, setdiff(names(data), variablen))])
}




