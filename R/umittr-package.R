#' Helper functions to ease the transition from SPSS to R
#' 
#' @name umittr-package
#' @aliases umittr
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
#' Items are scored from 1 to 5.
#'
#' \itemize{
#'   \item lz1 \emph{In den meisten Bereichen entspricht mein Leben meinen Idealvorstellungen}
#'   \item lz2 \emph{Meine Lebensbedingungen sind ausgezeichnet}
#'   \item lz3 \emph{Ich bin mit meinem Leben zufrieden}
#'   \item lz4 \emph{Bisher habe ich die wesentlichen Dinge erreicht, die ich mir für mein Leben wünsche.}
#'   \item lz5 \emph{Wenn ich mein Leben noch einmal leben könnte, würde ich kaum etwas ändern.}
#'   \item lezu \strong{Skala }\emph{Lebenszufriedenheit}
#'   \item e1 \emph{kontaktfreudig}
#'   \item e2 \emph{gesellig}
#'   \item e3 \emph{lebhaft}
#'   \item e4 \emph{temperamentvoll}
#'   \item extra \strong{Skala }\emph{Extraversion}
#'   \item n1 \emph{verletzbar}
#'   \item n2 \emph{empfindlich}
#'   \item n3 \emph{launenhaft}
#'   \item n4 \emph{selbstzweiflerisch}
#'   \item neuro \strong{Skala }\emph{Neurotizismus}
#'   }
#'
#' @docType data
#' @keywords datasets
#' @name pers_data
#' @usage data(pers_data)
#' @format A data frame with 345 rows and 16 variables
NULL


#' 18 items of a reactance questionnaire 
#'
#' The data comprises 18 items measuring reactance
#' and sex (i.e. 'Geschlecht')
#' Items are scored from 1 to 4.
#'
#' @docType data
#' @keywords datasets
#' @name laurasdaten
#' @usage data(laurasdaten)
#' @format A data frame with 524 rows and 19 variables
NULL



#' Intelligence, School grades and life data
#'
#' The data comprises items from the IST-2000-R
#' its verbal, figural, numerical intelligence scores
#' school grades from math, physics, german, etc. and GPA
#' plus life data like age, sex, location
#'
#' \itemize{
#'   \item vp \emph{VP ID}
#'   \item sex \emph{Geschlecht} (FACTOR)
#'   \item age \emph{Alter}
#'   \item verbint \emph{verbale Intelligenz}  
#'   \item numint \emph{num Intelligenz}
#'   \item figint \emph{fig Intelligenz}
#'   \item schlusint \emph{reasoning, schlussfolgerndes Denkes}
#'   \item se  an	gem	zr	re	rz	fa	wue	ma \emph{9 tests from the IST}
#'   Satzergaenzung, Analogien, Gemeinsamkeiten, Zahlenreihen, Rechenaufgaben, Rechenzeichen, Figurenauswahl, Wuerfelaufgaben, Matrizen
#'   \item geb_jahr Geburtsjahr
#'   \item abi Abischnitt
#'   \item bundesla	Bundesland (FACTOR)
#'   \item schulj	Anzahl der Schuljahre
#'   \item deu_m	Deutsch Note
#'   \item engl_m	Englisch Note
#'   \item math_m	Mathe Note
#'   \item phys_m	Physik Note
#'   \item chem_m	Chemie Note
#'   \item bio_m Bio Note
#'   \item 4 items of working memory (STOPRO, COORD, SWITCH, ATTENT)
#'   \item verbintsw \emph{verbale Intelligenz Standardwert}
#'   \item numintsw \emph{numerische Intelligenz Standardwert}
#'   \item figintsw \emph{figurale Intelligenz Standardwert}
#'   \item schluintsw \emph{schlussfolgerndes Denkes Standardwert}
#'   \item z_Auf  z_Koord z-transformierte Aufmerksamkeit und Koordination
#'   \item Produkt Produktterm z_Auf*z_Koord
#'   \item Produkt2 Produktterm ATTENT*COORD
#'   }
#'
#' @docType data
#' @keywords datasets
#' @name ist_data
#' @usage data(ist)
#' @format A data frame with 128 rows and 39 variables
NULL

