#' Helper functions to ease the transition from SPSS to R
#' 
#' @name lillionscage-package
#' @aliases lillionscage
#' @docType package
#' @title Helper functions to ease the transition from SPSS to R
#' @author \email{macx@@lillion.e4ward.com}
#' @keywords package
#' @seealso \code{\link{...}}
NULL

#' Statewide Crime Data
#'
#' The data are from Statistical Abstract of the United States and most
#' variables were measured in 1993.
#'
#' \itemize{
#'   \item \emph{sid} ID number
#'   \item \emph{state} state abbreviation
#'   \item \emph{crime} violent crime rate (per 100,000 people in population)
#'   \item \emph{murder} murder rate (per 100,000 people in population)
#'   \item \emph{pctmetro} percent in metropolitan areas
#'   \item \emph{pctwhite} percent white
#'   \item \emph{pcths} percent high school graduates
#'   \item \emph{poverty}  percent with income below the poverty level
#'   \item \emph{single} percent of families headed by a single parent
#' }
#'
#' @docType data
#' @keywords datasets
#' @name crime_data
#' @usage data(crime_data)
#' @format A data frame with 51 rows and 9 variables
#' @source A. Agresti and B. Finlay, STATISTICAL METHODS FOR THE SOCIAL SCIENCES, Prentice Hall, 3rd edition, 2008
NULL


#' Personality data with life satisfaction
#'
#' The data comprises items and sum-scores for Extraversion
#' Neuroticism and Life Satisfaction
#'
#' \itemize{
#'   \item lz1 \emph{In den meisten Bereichen entspricht mein Leben meinen Idealvorstellungen}
#'   \item lz2 \emph{Meine Lebensbedingungen sind ausgezeichnet}
#'   \item lz3 \emph{Ich bin mit meinem Leben zufrieden}
#'   \item lz4 \emph{Bisher habe ich die wesentlichen Dinge erreicht, die ich mir für mein Leben wünsche.}
#'   \item lz5 \emph{Wenn ich mein Leben noch einmal leben könnte, würde ich kaum etwas ändern.}
#'   \item lezu \emph{Lebenszufriedenheit}
#'   \item e1 \emph{kontaktfreudig}
#'   \item e2 \emph{gesellig}
#'   \item e3 \emph{lebhaft}
#'   \item e4 \emph{temperamentvoll}
#'   \item extra \emph{Extraversion}
#'   \item n1 \emph{verletzbar}
#'   \item n2 \emph{empfindlich}
#'   \item n3 \emph{launenhaft}
#'   \item n4 \emph{selbstzweiflerisch}
#'   \item neuro \emph{Neurotizismus}
#'   }
#'
#' @docType data
#' @keywords datasets
#' @name pers_data
#' @usage data(pers_data)
#' @format A data frame with 345 rows and 16 variables
NULL