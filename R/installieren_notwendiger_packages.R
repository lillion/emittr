#' @title Installieren notwendiger packages
#'
#' @description voellig unnoetige Funktion, aber praktisch
#' "car", "lmtest", "QuantPsyc", "lavaan", "lme4",
#' "semTools", "semPlot","GPArotation", "HH", "knitr", "emmeans",
#' "rockchalk"
#'  "sjPlot", "pander", "lattice",  "ggvis","ez","tidyverse", "haven"
#' früher noch: "psych", "ggplot2", "reshape2", "Hmisc", "perturb",
#' "Rcmdr"
#' @param update logisch: sollen gleichzeitig alle Pakete aktualisiert werden
#' @export
#' @keywords helper
#' @return nichts
#' @examples 
#' pakete_installieren()
pakete_installieren <- function () {
  wants <- c("car", "lmtest", "QuantPsyc", "lavaan", "lme4", "haven","readxl","psycho", "rockchalk",
             "psych", "ggplot2", "ggpubr", "GGally", "reshape2", "semTools", "semPlot","GPArotation", "emmeans",
             "HH", "Hmisc", "knitr", "sjPlot", "pander", "lattice", "perturb", "ggvis","ez","tidyverse",
             "effsize", "lsr")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has],dependencies=T)
  update.packages(ask = F)
  print(wants[!has])
}

	
