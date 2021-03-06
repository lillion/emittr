% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Normierung.R
\name{Tscorestransformieren}
\alias{Tscorestransformieren}
\title{T Werte eines Vektors berechen}
\usage{
Tscorestransformieren(x)
}
\arguments{
\item{x}{Vektor}
}
\value{
Dataframe
}
\description{
T Werte eines Vektors berechen
}
\examples{
x <- data.frame(x=rnorm(10000)) # Datensatz erzeugen
Tscorestransformieren(x$x) # in Normwerte umwandeln
}
