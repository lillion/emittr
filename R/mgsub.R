#' @title Buchstabenreihen ersetzen in Strings
#'
#' @description Ersetzt gleichzeitig mehrere Buchstaben in Zeichenketten, wobei die Positionen im Vektor
#' den jeweiligen Ersatz bestimmen, also 1. Stelle - 1. Stelle
#' 2. Stelle - 2. Stelle usw.
#' @param pattern das Muster nach Art von c("ä","ü","ö")
#' @param replacement Ersatzmuster nach Art von c("ae","ue","oe")
#' @param x Stringvektor, in dem die Ersetzung stattfinden soll
#' @param ... weitere Optionen für gsub
#' @export
#' @keywords gsub
#' @seealso \code{\link{gsub}}
#' @return string vektor
#' @examples \dontrun{
#' mgsub(c("ä","ü","ö","Ä","Ü","Ö","ß"), c("ae","ue","oe","Ae","Ue","Oe","ss"), beschriftung)
#'}
mgsub <- function(pattern, 
                  replacement, 
                  x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}