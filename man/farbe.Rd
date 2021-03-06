% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Farbpalette.R
\name{farbe}
\alias{farbe}
\title{Farbpalette für einfache Graphiken in R}
\usage{
farbe(Farbton, Schattierung)
}
\arguments{
\item{Farbton}{1:7, "blau", "grün", "gelb", "rot", "schwarz", "grau", "hellgrau"}

\item{Schattierung}{1:4, "voll", "dunkel", "mittel", "hell"}
}
\value{
Hexcode des Farbtons
}
\description{
Farbpalette für einfache Graphiken in R
}
\examples{
oldpar <- par(ask=F)
plot(1:7, 7:1, col=farbe(), pch=19, cex=6, xlab="", ylab="",
     xlim=c(0.5,7.5), ylim=c(-2.5,8), axes=FALSE)
points(1:7, 7:1-0.7, col=farbe(,2), pch=19, cex=6)
points(1:7, 7:1-1.4, col=farbe(,3), pch=19, cex=6)
points(1:7, 7:1-2.1, col=farbe(,4), pch=19, cex=6)
text(1:7, 7:1+0.7, paste("farbe(", 1:7, ")\n",dimnames(farbe())[[1]][1:7], sep=""), cex=0.9)
sapply(1:7,function(x) text(x,8-x-seq(0,2.1,length.out=4),paste(dimnames(farbe())[[2]][1:4]),cex=0.5))
par(oldpar)

farbe("rot","mittel")

hist(islands, col=farbe("blau","dunkel"))

plot(1:28, pch=16, cex=seq(5,15,length.out = 28), col=farbe())
plot(rnorm(1000),rnorm(1000), main="Scatterplot Example", col=farbe("rot","mittel"), pch=16)
points(rnorm(1000,.5),rnorm(1000),  col=farbe("blau","mittel"), pch=16)
}
