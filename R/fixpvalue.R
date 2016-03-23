#' @title Fix P-Values
#' 
#' @description 
#' A function to fix p-values in summaries for xtable. P-values should never display zero (mathematically), so this attempts to correct them. This will mostly be used for the coefficient section of summary tables from R when going into x-table
#' 
#' @param x The summary table to "fix" the p-values for
#' @param dig The number of digits you wish to round to
#' 
#' @export
#' 
#' @details Thus function expects summary tables (and turns them into a data.frame). It assumes that one column of the table contains p-values (with the name Pr..., normal in many summary tables). It will shoot a warning letting you know if you are using the function with a table that doesn't have a name that starts with this. The function takes the data frame and manipulates all values that round to 0 (at the set digits) to instead display that the p-value is less than the current rounded digits outcome. Note that this makes the last column of the output dataframe either character or numeric.
#' 
#' @examples 
#' #this gives a summary table with a small p-value
#' (mod <- coef(summary(lm(uptake ~ conc + Treatment + Type + Plant, data=CO2))))
#' 
#' #this fixes the p-value to 2 digits, correctly reporting p-values that would have been rounded to 0
#' fixp(mod, dig=2)
#' rm(mod)
#' @author e.d.

fixp <- function(x, dig=3){
  x <- as.data.frame(x)
  where <- which(substr(names(x),1,3)=="Pr(")
  if(length(where)==0)
    warning("No column's name  did start with Pr."); stop
  x[,where] <- round(x[,where], dig)
  for(i in 1:nrow(x)){
    if(x[i,where] == 0)
      x[i,where] <- paste0("<.", paste0(rep(0,dig-1), collapse=""), "1")
  }
  
  x
}
