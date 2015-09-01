#' @title Installieren notwendiger packages
#'
#' @description voellig unnoetige Funktion, aber praktisch
#' "car", "lmtest", "QuantPsyc", "lavaan", 
#' "semTools", "semPlot","GPArotation", "HH", "knitr","MplusAutomation",
#'  "sjPlot", "pander", "lattice",  "ggvis","ez","dplyr"
#' früher noch: "psych", "ggplot2", "reshape2", "Hmisc", "perturb",
#' "Rcmdr"
#' @param update logisch: sollen gleichzeitig alle Pakete aktualisiert werden
#' @export
#' @keywords helper
#' @return nichts
#' @examples 
#' pakete_installieren()
pakete_installieren <- function () {
  wants <- c("car", "lmtest", "QuantPsyc", "lavaan", "psych", "ggplot2", "reshape2", "semTools", "semPlot","GPArotation", "HH", "Hmisc", "knitr","MplusAutomation", "sjPlot", "pander", "lattice", "perturb", "ggvis","ez","dplyr")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has],dependencies=T)
  update.packages(ask = F)
  print(wants[!has])
}

	
