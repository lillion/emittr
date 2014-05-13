#' @title Einfacher univariater Bootstrap
#'
#' @description einfacher bootstrap für  univariate Statistik
#' @param x
#' @param stat 
#' @param reps 
#' @export
#' @keywords bootstrap
#' @seealso boot
#' @return bootstrap
#' @examples {
#' mydata<-rchisq(25,df=3)
#' simpleboot_d(mydata, "mean", reps=10000)
#' simpleboot_d(mydata, sd, reps=5000)
#'}
simpleboot_d<-function(x,stat,reps=1000) {
 cat("This simple function will not show you warning messages.\n")
 cat("Check results closely and be prepared to consult a statistician.\n")
 stat <- as.character(substitute(stat))
 if(stat=="max" | stat=="min") { warning("Bootstrap is likely to fail for minima and maxima") }
 require(boot)
 eval(parse(text=eval(substitute(paste("p.func<-function(x,i) ",stat,"(x[i])",sep=""),list(stat=stat)))))
 myboots<-boot(x,statistic=p.func,R=reps,stype="i")
 hist(myboots$t,breaks=25,main="Empirische Verteilung des bootstrap",xlab=paste("Statistik:",stat),ylab="Häufigkeit")
 abline(v=mean(myboots$t),col="blue")
 abline(v=myboots$t0, col="red")
 suppressWarnings(return(list(Anzahl_Samples=reps,Punkt.Schätzung=myboots$t0,normal.ci=c(boot.ci(myboots)$normal[2],boot.ci(myboots)$normal[3]),
percent.ci=c(boot.ci(myboots)$percent[4],boot.ci(myboots)$percent[5]),
bca.ci=c(boot.ci(myboots)$bca[4],boot.ci(myboots)$bca[5]))))
}
