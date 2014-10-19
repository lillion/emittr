#' @title Korrelationsplot für einfache Regression
#'
#' @description dient nur Lernzwecken
#' @param beta data 
#' @export
#' @keywords lm
#' @seealso lm
#' @return plot
#' @examples 
#' library(dplyr)
#' library(psych)
#' dfcov <- data_frame(punkte_intel=c(10,30,10,50,30,15),
#' punkte_klaus=c(10,19,30,50,60,20),dx=punkte_intel-mean(punkte_intel),
#' dy=punkte_klaus-mean(punkte_klaus),covZ=dx*dy,cov=covZ/(length(punkte_intel)-1))
#' library(manipulate)
#' manipulate(myPlotumit(beta,dfcov), beta = slider(-1, 1, step = 0.01))
myPlotumit <- function(beta, data){
  data <- data.frame(scale(data))
  y <- data[[2]]
  x <- data[[1]]
  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c("intell", "note", "häufigkeit")
  plot(
  x,y,
    xlab = "intell",
    ylab = "note",
  xlim=c(-1.5,2)
  )
  abline(0, beta , lwd = 2)
  points(0, 0, cex = 1.5, pch = 19)
  predicted <- x*beta
  
  segments(x,y, x,predicted)
  
  # add labels (res values) to points
  library(calibrate)
  res <- round(y-predicted,2)
  textxy(x, y, res, cex=0.7)
  
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", beta, "mse = ", round(mse, 4)))
}


