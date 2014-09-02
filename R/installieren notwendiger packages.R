#' @title Installieren notwendiger packages
#'
#' @description völlig unnötige Funktion, aber praktisch
#' @param keine Parameter
#' @export
#' @keywords helper
#' @return nichts
#' @examples 
#' pakete_installieren()
pakete_installieren <- function () {
  wants <- c("car", "lmtest", "QuantPsyc", "lavaan", "psych", "ggplot2", "reshape2", "semTools", "semPlot","GPArotation", "HH", "Hmisc", "knitr","MplusAutomation", "Rcmdr", "sjPlot", "pander", "lattice", "perturb", "ggvis","ez","dyplr")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has],dependencies=T)
  update.packages()
  print(wants[!has])
}

	
