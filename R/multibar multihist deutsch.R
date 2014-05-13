
#' @title Mehrere getrennte Histogramme
#'
#' @description produces a histogram for each variable in a dataframe or its subset, which optionally can be saved to disk as a pdf
#' @param x data.frame
#' @param variables Vektor der Variablen, die dargestellt werden sollen
#' @param labels String-Vektor der Labels, die verwendet werden sollen
#' @param xlab Unterschrift unter dem Plot
#' @param save Soll ein PDF gespeichert werden
#' @param ... zusätzliche Parameter von hist (z.B. col)
#' @export
#' @keywords umit
#' @seealso lillionscage::multibar
#' @return Histogramme
#' @examples \dontrun{ 
#' multihist(dd)
#' multibar(dd, 80:82, save = T)
#' multibar(x2,c(1:3))
#' multibar(neo_n,5,ylab="häu")
#'}

multihist_d <- function(x, variables, labels = TRUE, xlab = "Verteilung", save=TRUE,...) {
	if(class(x)!="data.frame") stop("x ist kein Datensatz")
	if(missing(variables)) variables=1:length(x)
	ll <- attributes(x)$variable.labels
	if(length(ll)==0) {
		cat("keine Labels im Datensatz, verwende daher stattdessen Variablennamen")
		ll <- names(x)
		}
	ll <- gsub("\\.", "", ll)
	ll <- sub("\\b(\\w)", "\\U\\1", ll, perl=TRUE)
	for (i in variables) {
		if (is.numeric(x[[i]])) {
			hist(x[[i]], main = ll[i], xlab = xlab, cex.main = 0.8,...)
				if (save) 
			dev.copy2pdf(file = paste0(ll[[i]], " (Histogramm).pdf"), height = 8, 
				width = 12)
				}
	}
}




#' @title Mehrere getrennte barplots
#'
#' @description given a dataframe, produces a barplot for each variable or a subset of variables, which optionally can be saved to disk as a pdf
#' @param x data.frame
#' @param variables Vektor der Variablen, die dargestellt werden sollen
#' @param labels String-Vektor der Labels, die verwendet werden sollen
#' @param xlab Unterschrift unter dem Plot
#' @param save Soll ein PDF gespeichert werden
#' @param ... zusätzliche Parameter von hist (z.B. col)
#' @export
#' @keywords umit
#' @seealso mulithist
#' @return Barplot
#' @examples \dontrun{
#' multi
#'}
multibar_d <- function(x, variables, labels, xlab = "Antworten", 
	save = TRUE,...) {
	if(class(x)!="data.frame") stop("x ist kein Datensatz")
	if(missing(variables)) variables=1:length(x)
	if (!missing(labels)) {
	 if(is.character(labels) & length(variables)==length(labels)) {
		ll <- labels
	} else {
		cat("Labels/Anzahl entsprechen nicht den Variablen\n")
		ll <- attributes(x)$variable.labels}} else {
	ll <- attributes(x)$variable.labels
	}
	if(length(ll)==0) {
		cat("keine Labels im Datensatz, verwende daher stattdessen Variablennamen")
		ll <- names(x)
		}
		
	ll <- gsub("\\.", "", ll)
	ll <- gsub("\\/","-",ll) # notfalls raus
	ll <- sub("\\b(\\w)", "\\U\\1", ll, perl=TRUE)
	for (i in variables) {
		labelnum <- which(i==variables)
		barplot(table(x[[i]]), main = ll[labelnum], xlab = xlab, cex.main = 0.8, ...)
		if (save) 
			dev.copy2pdf(file = paste0(ll[[labelnum]], " (Barplot).pdf"), height = 8, 
				width = 12,family="URWPalladio")
	}
	#if (save) dev.off()
}



