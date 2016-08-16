rr <- function(l) {
  r1 <- (function(x) {temp=summary(x); f=temp$fstatistic;c(R=sqrt(temp$r.squared),"R\U00B2"=temp$r.squared, p = unname(pf(f[1],f[2],f[3],lower.tail=FALSE)),"adj R\U00B2"=temp$adj.r.squared,"pred R\U00B2"=lm_pred_r_squared(x))})(l) 
  
  #r2 <- anova(l)
  
  #r2 <- r2[c(5,3,1,6)]
  f <- summary(l)$fstatistic#[c(2,3,1)]
  f <- c(summary(l)$sigma,f)
  names(f) <- c("Res SE","F","df1","df2")
  #p <- pf(f[1],f[2],f[3],lower.tail=FALSE)
  #r2[1,] <- c(summary(ll[[1]])$fstatistic,p)
  # rch <- c(r1[1,2],diff(r1[,2]))
  #data.frame(t(r1),t(f), row.names="Model")
    temp <- c(r1,f)
  temp1=data.frame(A=NA,B=NA,C=NA,D=NA,E=NA,F=NA,G=NA)[numeric(0), ]
  temp1=rbind(temp1,temp)
  names(temp1) <- names(temp)
  if(round(temp1$p,3)==0) temp1$p <- "<.001" ## neu
  row.names(temp1) <- "Model"
  temp1
}



round.df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}



qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)+theme_bw()
  
}