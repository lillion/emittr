#' Missinganalyse pro Zeile
#'
#' @param data Datensatz
#' @param logisch logisch: sollen ein logischer Vektor ausgegeben werden (standard=FALSE)
#' 
#'
#' @return Fallnummern bzw. Ja/Nein
#' @export
#'
#' @examples
#' demoframe <- data.frame(sapply(1:10,function(x) rnorm(30,100,10)))
#' demoframe <- as.data.frame(lapply(demoframe, function(x) "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, .2))))))
#' demoframe
#' fall.hat.missings(demoframe) # Fallnummern mit Missings
#' fall.hat.missings(demoframe,logisch = T) # Vektor T/F für Fälle mit und ohne Missings
#' anzahlmissings(demoframe)
fall.hat.missings <- function (data, logisch=FALSE) {
  r <- apply(data,1, function(x) {sum(is.na(x))})
  if(!logisch) return(which(r!=0))
  return(r!=0)
}

#' Anzahl der Missings pro Fall
#'
#' @param data Datensatz
#' 
#'
#' @return Anzahl der Missings pro Fall
#' @export
#'
#' @examples
#' demoframe <- data.frame(sapply(1:10,function(x) rnorm(30,100,10)))
#' demoframe <- as.data.frame(lapply(demoframe, function(x) "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, .2))))))
#' demoframe
#' anzahlmissings(demoframe) # Anzahl Missings pro Fall
anzahlmissings <- function(data){
  r <- apply(data,1, function(x) {sum(is.na(x))})
  return(r)
}
