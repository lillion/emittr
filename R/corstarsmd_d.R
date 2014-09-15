#' @title Korrelationstabelle mit Signifikanzsternen
#'
#' @description die Funktion errechnet eine Korrelationstabelle aus den übermittelten Variablen und fügt, wo angebracht, Signifikanzsterne hinzu
#' @param x Datensatz (dataframe) Objekt mit den Variablen
#' @param type Ausgabevarianten ohne, html or markdown (übermittelt Tabelle an kable)
#' @param stellen Anzahl Stellen nach dem Komma
#' @param zehnproz (logisch) soll p<.10 mit einem Kreuz angezeigt werden
#' @param abk (logisch) sollen Spaltennamen gekürzt werden
#' @param diagonale (logisch) soll die Diagonale gefüllt werden
#' @export
#' @keywords correlation
#' @seealso \link{cor} 
#' @return text table
#' @examples 
#' corstarsmd_d(pers_data[c(7:10,12:15)],type="ohne", stellen=2, zehnproz=TRUE)
#' 
#' \dontrun{
#' # um das Ergebnis z.B. in Microsoft Word einzufügen, folgende Schritte ausführen:
#' for_word <- corstarsmd_d(pers_data,type = "ohne",stellen=2) # Ausgabe in Objekt sichern
#' library(xlsx) # Excel library laden
#' write.xlsx(for_word, "test.xlsx") # Ausgabe in ein Excel-Dokument speichern
#' # Das gespeicherte Dokument kann in Excel geöffnet werden, 
#' # von dort Tabelle kopieren und in Word einfügen
#' }

corstarsmd_d <- function(x, type="markdown", stellen=3, zehnproz=F, abk=T, diagonale=F,  ...){ 
  suppressPackageStartupMessages(require(Hmisc)) 
  suppressPackageStartupMessages(require(knitr))
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  if (zehnproz){
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .2, "✝  ", "   "))))
  } else {
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", "   ")) ) }
  R <- format(round(cbind(rep(-1.111, ncol(x)), R), stellen ))[,-1] 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), "   ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), " ", sep="") 
  if (abk) colnames(Rnew) <- abbreviate(colnames(Rnew), minlength = stellen + 3)
  Rnew <- as.data.frame(Rnew) 
  if (nrow(Rnew) == ncol(Rnew)) {
    Rnew[!lower.tri(Rnew, diag = diagonale)] <- ""
  }
  if(type=="ohne") return(Rnew) 
  return(kable(Rnew,format=type,...)) 
}