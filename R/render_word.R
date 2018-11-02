#' Rendern als Word-Dokument
#'
#' @param file das R Skript, das gerendert werden soll
#' @description Diese Funktion wandelt ein R-Skript in eine Word-Datei um. Dafür ist das Paket "rmarkdown" notwendig!
#' @return Word Doc
#' @export
#'
#' @examples
#' KI(wert = 66,standardabweichung = 10,reliabilitaet = .98,sicherheit = .99,seitigkeit = 1,mittelwert = 50,typ = "r") # inklusive Regressionshypothese
#' KI(wert = 66,standardabweichung = 10,reliabilitaet = .96,sicherheit = .8,seitigkeit = 1,mittelwert = 50,typ = "r") # inklusive Regressionshypothese
wordrender <- function(file, ...){
  if(!file.exists(file)) stop("Datei gibt es nicht!")
  rmarkdown::render(file,output_format = "word_document")
}




