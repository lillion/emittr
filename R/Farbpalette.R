#' Farbpalette für einfache Graphiken in R 
#'
#' @param Schattierung 1:4, "voll", "dunkel", "mittel", "hell"
#' @param Farbton 1:7, "blau", "grün", "gelb", "rot", "schwarz", "grau", "hellgrau"
#'
#' @return Hexcode des Farbtons
#' @export
#'
#' @examples
#' oldpar <- par(ask=F)
#' plot(1:7, 7:1, col=farbe(), pch=19, cex=6, xlab="", ylab="",
#'      xlim=c(0.5,7.5), ylim=c(-2.5,8), axes=FALSE)
#' points(1:7, 7:1-0.7, col=farbe(,2), pch=19, cex=6)
#' points(1:7, 7:1-1.4, col=farbe(,3), pch=19, cex=6)
#' points(1:7, 7:1-2.1, col=farbe(,4), pch=19, cex=6)
#' text(1:7, 7:1+0.7, paste("farbe(", 1:7, ")\n",dimnames(farbe())[[1]][1:7], sep=""), cex=0.9)
#' sapply(1:7,function(x) text(x,8-x-seq(0,2.1,length.out=4),paste(dimnames(farbe())[[2]][1:4]),cex=0.5))
#' par(oldpar)
#' 
#' farbe("rot","mittel")
#' 
#' hist(islands, col=farbe("blau","dunkel"))
#' 
#' plot(1:28, pch=16, cex=seq(5,15,length.out = 28), col=farbe())
#' plot(rnorm(1000),rnorm(1000), main="Scatterplot Example", col=farbe("rot","mittel"), pch=16)
#' points(rnorm(1000,.5),rnorm(1000),  col=farbe("blau","mittel"), pch=16)


farbe=function(Farbton, Schattierung){
COL <- structure(c("#569BBD", "#4C721D", "#F4DC00", "#F05133", "#000000", 
"#808080", "#D9D9D9", "#569BBDC0", "#4C721DC0", "#F4DC00C0", 
"#F05133C0", "#000000C0", "#808080C0", "#D9D9D9C0", "#569BBD80", 
"#4C721D80", "#F4DC0080", "#F0513380", "#00000080", "#80808080", 
"#D9D9D980", "#569BBD40", "#4C721D40", "#F4DC0040", "#F0513340", 
"#00000040", "#80808040", "#D9D9D940"), .Dim = c(7L, 4L), .Dimnames = list(c("blau", "grün", "gelb", "rot", "schwarz", "grau", "hellgrau"), c("voll", "dunkel", "mittel", "hell")))
  return(COL[Farbton, Schattierung])
}