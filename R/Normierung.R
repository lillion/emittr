#' Stanine Werte berechnen
#'
#' @param x 
#'
#' @return Dataframe mit den Grenzwerten für die Stanine
#' @export
#'
#' @examples
#' x <- rnorm(10000) # Datensatz erzeugen
#' stanine(x)
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
#' staninenormierung(x)
#' table(staninenormierung(x))
#' hist(staninenormierung(x),breaks=0:9,xlim=c(0,9))
staninenormierung <- function(x){
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
#' Tscores(x$x) # Normwerte erzeugen
Tscores <- function(x){
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
#' @examples
#' x <- data.frame(x=rnorm(10000)) # Datensatz erzeugen
#' x$i <- Tscores(x$x) # Normwerte erzeugen
#' RohwertezuNormwerten(Rohwerte = x["x"],Normwerte = x["i"])
#' y <- x[RohwertezuNormwerten(Rohwerte = x$x,Normwerte = x["i"])$rows,]
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


