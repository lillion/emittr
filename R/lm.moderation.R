#' @title calculates slopes and intercepts for a moderation and plots a simple graph 
#'
#' @description calculates the slopes and intercepts for a simple moderation analysis from a lm
#' @param mod the result of a moderation 
#' done like this: lm(y~x*z), so that you have one predictor x
#' and one moderator z
#' @param zsd the standard deviation of z to calculate the slopes
#' @param ... Parameter for plot (xlab, ylab, main etc.)
#' @export
#' @keywords lm
#' @seealso \code{\link{lm}}
#' @return moderation
#' @examples 
#' data(crime_data)
#' attach(crime_data)
#' xm <- poverty-mean(poverty, na.rm=T)
#' zm <- pcths-mean(pcths, na.rm=T)
#' zz <- xm*zm
#' mod <- lm(crime~poverty+pcths+zz, data=crime_data)
#' moderation.slopes(mod, xlab="crime rate")
#' detach(crime_data)
moderation.slopes <- function (mod, 
                               zsd = 1,
                               mod_name="Moderator",
                               ...) 
{
  df <- mod$model
  names(df) <- c("y","x","z")
  z <- df$z
  z <- if(round(mean(z, na.rm=T)==0)) z else z - mean(z, na.rm = TRUE)
  
  zhi <- mean(z, na.rm = TRUE) + zsd * sd(z, na.rm = TRUE)
  zlo <- mean(z, na.rm = TRUE) - zsd * sd(z, na.rm = TRUE)
  zme <- mean(z, na.rm = TRUE)
  b0 <- summary(mod)$coef[1, 1]
  b1 <- summary(mod)$coef[2, 1]
  b2 <- summary(mod)$coef[3, 1]
  b3 <- summary(mod)$coef[4, 1]
  x.zhi <- (b1 + b3 * zhi)
  x.zlo <- (b1 + b3 * zlo)
  x.zme <- (b1 + b3 * zme)
  int.zhi <- (b0 + b2 * zhi)
  int.zlo <- (b0 + b2 * zlo)
  int.zme <- (b0 + b2 * zme)
  seb.zhi <- sqrt(vcov(mod)[2, 2] + 2 * zhi * vcov(mod)[2, 
                                                        4] + zhi^2 * vcov(mod)[4, 4])
  seb.zlo <- sqrt(vcov(mod)[2, 2] + 2 * zlo * vcov(mod)[2, 
                                                        4] + zlo^2 * vcov(mod)[4, 4])
  seb.zme <- sqrt(vcov(mod)[2, 2] + 2 * zme * vcov(mod)[2, 
                                                        4] + zme^2 * vcov(mod)[4, 4])
  td <- qt(0.975, df = summary(mod)$df[2])
  zhi.u <- x.zhi + td * seb.zhi
  zhi.l <- x.zhi - td * seb.zhi
  zlo.u <- x.zlo + td * seb.zlo
  zlo.l <- x.zlo - td * seb.zlo
  zme.u <- x.zme + td * seb.zme
  zme.l <- x.zme - td * seb.zme
  mat <- matrix(NA, 3, 5)
  colnames(mat) <- c("INT", "Slope", "SE", "LCL", "UCL")
  rownames(mat) <- c("at zHigh", "at zMean", "at zLow")
  mat[1, ] <- c(int.zhi, x.zhi, seb.zhi, zhi.l, zhi.u)
  mat[2, ] <- c(int.zme, x.zme, seb.zme, zme.l, zme.u)
  mat[3, ] <- c(int.zlo, x.zlo, seb.zlo, zlo.l, zlo.u)
  mat <- data.frame(mat)
  #plot(-2:2, -2:2, type = "n")
  df$color <- cut(z,breaks = c(-Inf, -zsd*sd(z), zsd*sd(z), Inf),right = F, labels=c(2,1,4))
  df$color <- ifelse(df$color==1,1,ifelse(df$color==4,4,2))
  cat("categories: middle, low, high\n")
  print(table(df$color))
  plot(df[[2]], df[[1]],col=df$color,pch=20, cex=.7, cex.axis=.6, frame.plot=F, ...)
  mapply(function(x,y,z) abline(x,y,col=z), mat[,1], mat[,2],c("blue","black","red"), SIMPLIFY = F)
  legend(x="topleft",c(paste(mod_name,"+",zsd,"SD"), paste(mod_name,"middle"), paste(mod_name,"-",zsd,"SD")), lty = 1, col = c(4,1, 2),bty="n")
  return(data.frame(mat))
}



