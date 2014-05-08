
#' @title <brief desc>
#'
#' @description <full description>
#' @param x
#' @param variables
#' @param labels
#' @param xlab
#' @export
#' @keywords umit
#' @seealso multibar
#' @return none
#' @examples \dontrun{ 
#' multihist(dd)
#' multibar(dd, 80:82, save = T)
#' multibar(x2,c(1:3))
#' multibar(neo_n,5,ylab="häu")
#'}

multihist <- function(x, variables, labels = TRUE, xlab = "Verteilung",save=TRUE,...) {
	if(class(x)!="data.frame") stop("x is not a Dataframe")
	if(missing(variables)) variables=1:length(x)
	ll <- attributes(x)$variable.labels
	if(length(ll)==0) {
		cat("no Labels in Dataframe, using variable names instead")
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





multibar <- function(x, variables, labels, xlab = "Antworten", 
	save = TRUE,...) {
	if(class(x)!="data.frame") stop("x is not a Dataframe")
	if(missing(variables)) variables=1:length(x)
	if (!missing(labels)) { if(is.character(labels) & length(variables)==length(labels)) {
		ll <- labels
	} else {
		cat("Labels don't correspond to variables\n")
		ll <- attributes(x)$variable.labels}} else {
	ll <- attributes(x)$variable.labels
	}
	if(length(ll)==0) {
		cat("no Labels in Dataframe, using variable names instead\n")
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



