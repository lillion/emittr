#' Variablen-Reihenfolge ändern
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
  if (!class(variablen)=="character") stop("Variable müssen als Strings angegeben werden!")
  return( data[, c(refcols, setdiff(names(data), refcols))])
}



