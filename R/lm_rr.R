

rr <- function(l) {
  r1 <- (function(x) {temp=summary(x); f=temp$fstatistic;c(R=sqrt(temp$r.squared),rsq=temp$r.squared, p = unname(pf(f[1],f[2],f[3],lower.tail=F)),adjR2=temp$adj.r.squared,predR2=lm_pred_r_squared(x))})(l) 
  
  #r2 <- anova(l)
  
  #r2 <- r2[c(5,3,1,6)]
  f <- summary(l)$fstatistic[c(2,3,1)]
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  #r2[1,] <- c(summary(ll[[1]])$fstatistic,p)
  # rch <- c(r1[1,2],diff(r1[,2]))
  data.frame(t(r1),t(f), row.names="Model")
  
}