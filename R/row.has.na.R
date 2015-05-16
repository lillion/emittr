#' Missinganalyse pro Zeile
#'
#' @param data Datensatz
#' @param which logisch: sollen nur die Fallnummern ausgegeben werden
#' @param Zeile logisch: Missings pro Zeile anzeigen
#' 
#'
#' @return Fallnummern bzw. Ja/Nein
#' @export
#'
#' @examples
#' demoframe <- data.frame(sapply(1:15,function(x) rnorm(50,100,10)))
#' demoframe <- as.data.frame(lapply(demoframe, function(x) "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, .2))))))
#' row.has.na(demoframe)
row.has.na <- function (data, which=TRUE, zeile=FALSE) {
  r <- apply(data,1, function(x) {sum(is.na(x))})
  if(zeile) return(r)
  if(which) return(which(r!=0))
  return(r!=0)
}