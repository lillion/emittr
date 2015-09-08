#' Stanine Werte berechnen
#'
#' @description Bestimmt die Staninewert-Grenzen eines gegebenen Vektors. Praktisch für die Normierung eines neuen Tests.
#' Die Normierung erfolgt nach den Vorschlägen von Telt und Stelzl.
#' @param x 
#'
#' @return Dataframe mit den Grenzwerten für die Stanine
#' @export
#' @references Tent, L., Stelzl, I. (1993), Pädagogisch-psychologische Diagnostik: Theoretische und methodische Grundlagen. Göttingen: Hogrefe.
#' @keywords Stanine, Normierung
#' @examples
#' IQ <- rnorm(20000,100,15) # Datensatz erzeugen
#' stanine(IQ)
#' 
stanine=function(x){
  probs_l= c(0, 4, 11,23,40,60,77,89,96)/100
  probs_h=c(4,11,23,40,60,77,89,96,100)/100
  output=data.frame(Stanine=1:9, von=quantile(x, probs_l), bis=quantile(x, probs_h))
  rownames(output) <- c("0%-4%", "4%-11%", "11%-23%", "23%-40%", "40%-60%", "60%-77%", "77%-89%", "89%-96%", "96%-100%")
  return(output)
}




#' Werten Stanine Normierung zuweisen
#'
#' @param x 
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' x <- rnorm(10000) # Datensatz erzeugen
#' y <- c(1.2,-1,.4,3) # für diese Werte 
#' staninentransformieren(x)
#' table(staninentransformieren(x))
#' hist(staninentransformieren(x),breaks=0:9,xlim=c(0,9))
#' hist(exp(x)) # schief verteilte Daten
#' x <- staninentransformieren(exp(x)) 
#' table(staninentransformieren(x))
staninentransformieren <- function(x){
  probs= c(0,4,11,23,40,60,77,89,96,100)/100
  y <- findInterval(x, quantile(x,  probs), all.inside = TRUE)
  return(y)
}



#' T Werte eines Vektors berechen
#'
#' @param x Vektor
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' x <- data.frame(x=rnorm(10000)) # Datensatz erzeugen
#' Tscorestransformieren(x$x) # in Normwerte umwandeln
Tscorestransformieren <- function(x){
  z <- scale(x,center = TRUE,scale = TRUE)
  # z2 <- (x-mean(x))/sd(x)
  t <- z*10+50
  t <- as.numeric(t)
  return(round(t,0))
}


#' Rohwerten Normwerte zuordnen
#'
#' @param Rohwerte 
#' @param Normwerte 
#'
#' @return Dataframe
#' @export
#'
#' @references Tent, L., Stelzl, I. (1993), Pädagogisch-psychologische Diagnostik: Theoretische und methodische Grundlagen. Göttingen: Hogrefe.
#' @examples
#' x <- data.frame(x=rnorm(10000)) # Datensatz erzeugen
#' x$T <- Tscorestransformieren(x$x) # in T-Werte umwandeln
#' RohwertezuNormwerten(Rohwerte = x["x"],Normwerte = x["T"])
#' x$Stanine <- staninentransformieren(x$x) # in Stanine umwandeln
#' y <- x[RohwertezuNormwerten(Rohwerte = x$x,Normwerte = x["T"])$rows,]
#' y
RohwertezuNormwerten <- function(Rohwerte, Normwerte){
  #stopifnot(length(Rohwerte)==length(Normwerte))
  ifelse(is.data.frame(Rohwerte) || is.data.frame(Normwerte),{
    frame=cbind(Rohwerte,Normwerte)
    names(frame) <- c("Rohwerte", "Normwerte")
  }, {
    frame <- data.frame(Rohwerte, Normwerte)
  })
 
    reihen <- as.integer(row.names(unique(frame["Normwerte"])))
  y <- frame[reihen,]
  y <- y[order(y$Normwerte),]
  y$rows <- rownames(y)
  rownames(y) <- NULL
  return(y)
}


