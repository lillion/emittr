#' @title Installieren notwendiger packages
#'
#' @description voellig unnoetige Funktion, aber praktisch
#' "car", "lmtest", "QuantPsyc", "lavaan", "psych", "ggplot2", "reshape2",
#' "semTools", "semPlot","GPArotation", "HH", "Hmisc", "knitr","MplusAutomation",
#' "Rcmdr", "sjPlot", "pander", "lattice", "perturb", "ggvis","ez","dplyr"
#' @param keine Parameter
#' @export
#' @keywords helper
#' @return nichts
#' @examples 
#' pakete_installieren(update=FALSE)
pakete_installieren <- function (update=FALSE) {
  wants <- c("psych","car", "lmtest", "QuantPsyc", "lavaan", "psych", "ggplot2", "reshape2", "semTools", "semPlot","GPArotation", "HH", "Hmisc", "knitr","MplusAutomation", "sjPlot", "pander", "lattice", "perturb", "ggvis","ez","dplyr", "yaml","rmarkdown")
  repos=repos = c(CRAN = "http://cran.rstudio.com")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has],dependencies=TRUE,repos = repos)
  if(update) update.packages(ask = F, repos=repos)
  print(wants[!has])
}

	
