#' erstes Setup von R fuer das RProfile
#'
#' @description Mit dieser einfachen Funktion ohne Parameter wird für den Benutzer in seinem Heimverzeichnis die .Rprofile Datei erstellt, die bei jedem Start von R aufgerufen wird.
#' Darin sind Voreinstellungen enthalten, die somit nicht mehr jedes Mal per Hand vorgenommen werden müssen.
#' \itemize{
#' \item library psych, ggplot2, foreign, dplyr, umittr werden geladen
#' \item options(stringsAsFactors=FALSE)
#' \item options(width=75, digits=3) # Zeilenbreite und Kommastellen
#' \item options(show.signif.stars=TRUE) # Signifikanzsterne
#' \item o() # öffent im Explorer/Finder das aktuelle Verzeichnis
#' \item plotstart() # entfernt den Hintergrund für Plots
#' \item Schriftfamilien für Graphiken
#' }
#' @return schreibt eine User-Datei auf die Festplatte. 
#' Falls die Datei '.Rprofile' schon besteht, wird ein Backup
#' der alten Daten erzeugt.
#' @export
#'
#' @examples
#' setup()
#' # wenn es funktioniert hat:
#' o() # öffnet das aktuelle Verzeichnis
setup <- function() {
if(Sys.info()[1]=="Darwin") {
  path="~/.Rprofile"
  if(file.exists(path)) file.rename("~/.Rprofile","~/Rprofile_backup")
  file.create(path)
  }
if(Sys.info()[1]=="Windows") {
#  path=R.home(component = "etc")
#  
#  file.create(path,"RProfile.site")
  path <- Sys.getenv("USERPROFILE")
  path=file.path(path,"Documents",".Rprofile")
  file.create(file.path(path))
} 

cat("## Praktisches .Rprofile
options(width=75, digits=3) # wie viele Stellen?
options(show.signif.stars=TRUE) # sollen die Sterne bei Signifikanzen stehen? sonst =FALSE

## Pakete laden
try(library(psych))
#notfalls in den folgenden Zeilen das # am Zeilenanfang löschen, damit die Befehle ausgeführt werden
try(library(ggplot2)) #ggplot2 ist für Graphiken, nicht unbedingt notwendig
try(library(haven)) #um SPSS Dateien zu laden
try(library(tidyverse)) # dplyr für viele Funktionen der Datenmanipulation
try(library(umittr)) # umittr Package laden
try(library(car)) # car Package laden
try(library(sjPlot)) # car Package laden
# try(library(readxl)) # Excel Dateien lesen

ggplot2::theme_set(theme_bw())

## Do you want to automatically convert strings to factor variables in a data.frame?
## WARNING!!! This makes your code less portable/reproducible.
options(stringsAsFactors=FALSE)


## CRAN mirror festlegen
options(\"repos\" = c(CRAN = \"https://cran.rstudio.com/\"))

## Create a new invisible environment for all the functions to go in so it doesn't clutter your workspace.
.env <- new.env()

## Returns a logical vector TRUE for elements of X not in Y
#.env$\"%nin%\" <- function(x, y) !(x %in% y) 

## Returns names(df) in single column, numbered matrix format.
.env$nn <- function(df) matrix(names(df)) 

## Single character shortcuts for summary() and head().
.env$s <- base::summary
.env$h <- utils::head

## ht==headtail, i.e., show the first and last 10 items of an object
.env$ht <- function(d) rbind(head(d,10),tail(d,10))

## Show the first 5 rows and first 5 columns of a data frame or matrix
.env$hh <- function(d) if(class(d)==\"matrix\"|class(d)==\"data.frame\") d[1:5,1:5]

## Read data on clipboard.
.env$read.cb <- function(...) {
  ismac <- Sys.info()[1]==\"Darwin\"
  if (!ismac) read.table(file=\"clipboard\", ...)
  else read.table(pipe(\"pbpaste\"), ...)
}

## Strip row names from a data frame (stolen from plyr)
.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

## List objects and classes (from @_inundata)
.env$lsa <- function() {
  obj_type <- function(x) { class(get(x)) }
  foo=data.frame(sapply(ls(envir=.GlobalEnv),obj_type))
  foo$object_name=rownames(foo)
  names(foo)[1]=\"class\"
  names(foo)[2]=\"object\"
  return(unrowname(foo))
}

## List all functions in a package (also from @_inundata)
.env$lsp <-function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  ls(
    pos = paste(\"package\", package, sep = \":\"),
    all.names = all.names,
    pattern = pattern
  )
}

## Open Finder to the current directory on mac
.env$macopen <- function(...) if(Sys.info()[1]==\"Darwin\") system(\"open .\")
.env$o       <- function(...) if(Sys.info()[1]==\"Darwin\") system(\"open .\")

if(Sys.info()[1]==\"Windows\") .env$o       <- function(...) shell(\"explorer .\")

# start plots
.env$plotstart <- function(...) {par(family=\"calibrilight\");par(bty=\"n\")} 

## Attach all the variables above
attach(.env)

## .First() run at the start of every R session. 
## Use to load commonly used packages? 
.First <- function() {
  if(Sys.info()[1]==\"Darwin\") {
    grDevices::quartzFonts(avenir = c(\"Avenir Book\", \"Avenir Black\", \"Avenir Book Oblique\", 
\"Avenir Black Oblique\"), helvetica = c(\"Helvetica Neue Light\", \"Helvetica Neue Bold\", 
\"Helvetica Neue Light Italic\", \"Helvetica Neue Bold Italic\"), 
gill=c(\"Gill Sans Light\",\"Gill Sans SemiBold\", \"Gill Sans Light Italic\", 
\"Gill Sans SemiBold Italic\"), 
skia=c(\"Skia-Regular\", \"Skia-Regular_Light-Condensed\", 
\"Skia-Regular_Condensed\", \"Skia Bold\"))

cat(\"\nfolgende Schriftfamilien erstellt:\n - 'avenir' \n - 'helvetica'\n - 'gill'\n - 'skia'\n\")
  }
  if(Sys.info()[1]==\"Windows\"){
grDevices::windowsFonts(calibrilight=\"TT Calibri Light\", 
arialnar=\"Arial Narrow\",century=\"TT Century Gothic\",
modern=\"TT Modern\", cambria=\"Cambria\")
cat(\"\nfolgende Schriftfamilien erstellt:\n - 'calibrilight' \n - 'arialnar'\n - 'century'\n - 'modern'\n - 'cambria'\n\")
}
  cat(\"\n.Rprofile erfolgreich geladen am\", date(), \"\n\")
  cat(\"\n   Willkommen zu UMIT R!\n\n\")
  cat(\"\n   Pakete psych, ggplot2, haven, tidyverse, umittr, car, sjPlot, dplyr geladen. \n
  Thema von ggplot2 auf theme_bw() gesetzt. \n\")

}


## .Last() run at the end of the session
.Last <- function() {
  # save command history here?
  cat(\"\n   Bussi und Baba!\n\n\")
}",file=file.path(path))

if(file.exists(file.path(path,".Rprofile"))) cat("\n.Rprofile erstellt.\nüberprüfen des Inhalts mit file.edit(\"",file.path(path,".Rprofile"),".Rprofile\")")
}

