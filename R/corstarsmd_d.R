#' @title Korrelationstabelle mit Signifikanzsternen
#'
#' @description die Funktion errechnet eine Korrelationstabelle aus den übermittelten Variablen und fügt, wo angebracht, Signifikanzsterne hinzu
#' @param x Datensatz (dataframe) Objekt mit den Variablen
#' @param type Ausgabevarianten ohne, html or markdown (übermittelt Tabelle an kable)
#' @param stellen Anzahl Stellen nach dem Komma
#' @param zehnproz (logisch) soll p<.10 mit einem Kreuz angezeigt werden
#' @param abk (logisch) sollen Spaltennamen gekürzt werden
#' @param diagonale (logisch) soll die Diagonale gefüllt werden
#' @param ... andere Parameter für \link{kable}, falls type="html" oder "markdown" 
#' @export
#' @keywords korrelation
#' @seealso \link{cor} 
#' @return text table
#' @examples 
#' data(pers_data)
#' corstarsmd_d(pers_data[c(7:10,12:15)],type="ohne", stellen=2, zehnproz=TRUE, untere=FALSE)
#' 
#' \dontrun{
#' # um das Ergebnis z.B. in Microsoft Word einzufügen, folgende Schritte ausführen:
#' for_word <- corstarsmd_d(pers_data,type = "ohne",stellen=2) # Ausgabe in Objekt sichern
#' library(xlsx) # Excel library laden
#' write.xlsx(for_word, "test.xlsx") # Ausgabe in ein Excel-Dokument speichern
#' # Das gespeicherte Dokument kann in Excel geöffnet werden, 
#' # von dort Tabelle kopieren und in Word einfügen
#' }

corstarsmd_d <- function(x, type="markdown", stellen=3, zehnproz=FALSE, abk=TRUE, diagonale=FALSE, untere=TRUE, ...){ 
  temp <- names(x)
  x <- x[sapply(x,is.numeric)]
  if(!identical(temp,names(x))) cat("Nicht numerische Variable(n):\n",setdiff(temp,names(x)),"\nwurde(n) entfernt.\n\n", temp[])
  x <- as.matrix(x) 
  R <- psych::corr.test(x)$r # psych
  p <- psych::corr.test(x)$p # psych
  if (zehnproz){
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", ifelse(p < .1, "\u2020  ", "   "))))
  } else {
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")) ) }
  R <- format(round(cbind(rep(-1.111, ncol(x)), R), stellen ))[,-1] 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), "   ", sep="") 
  colnames(Rnew) <- paste(colnames(x), " ", sep="") 
  if (abk) colnames(Rnew) <- abbreviate(colnames(Rnew), minlength = stellen + 3)
  Rnew <- as.data.frame(Rnew) 
  if (nrow(Rnew) == ncol(Rnew)) {
    Rnew <- sapply(Rnew, as.character)
    if(untere) Rnew[!lower.tri(Rnew, diag = diagonale)] <- " " else Rnew[!upper.tri(Rnew, diag = diagonale)] <- " " 
    Rnew <- as.data.frame(Rnew) 
  }
  rownames(Rnew) <- colnames(x) 
  if(untere) Rnew <- Rnew[, -length(Rnew[1, ])] else Rnew <- Rnew[-length(Rnew[,1 ]), ]
  if(type=="ohne") return(Rnew) 
  if("knitr" %in% rownames(installed.packages()) == FALSE) {return("Knitr fehlt! Bitte installieren!")}
  return(knitr::kable(Rnew,format=type,...)) 
}