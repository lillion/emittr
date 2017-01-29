#' Standardfehler und Konfidenzintervall des Mittelwerts, auch gruppenbezogen
#'
#' @param data 
#' @param measurevar 
#' @param groupvars 
#' @param na.rm 
#' @param conf.interval 
#' @param .drop 
#'
#' @return
#' @export
#'
#' @examples
#' summarySE(crime_data,"pcths")
#' summarySE(mtcars,"mpg",groupvars="cyl", conf.interval = 80)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  #library(plyr)
  if(!("plyr" %in% rownames(installed.packages()))) stop("plyr Package fehlt") 
  if(conf.interval>1) conf.interval=conf.interval/100
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  datac <- plyr::rename(datac, c("ci" = paste0("ci",conf.interval*100,"%")))
  return(datac)
}