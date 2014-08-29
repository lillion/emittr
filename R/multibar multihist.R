
#' @title Multiple separate histograms
#'
#' @description produces a histogram for each variable in a dataframe or its subset, which optionally can be saved to disk as a pdf
#' @param x data.frame
#' @param variables vector of variables to be used
#' @param labels character vector of labels
#' @param xlab
#' @param save should pdf be saved to disk
#' @param ... additional graphic parameters for hist (e.g., col)
#' @export
#' @keywords umit
#' @seealso \code{\link{multibar}}
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





#' @title Multiple separate barplots
#'
#' @description given a dataframe, produces a barplot for each variable or a subset of variables, which optionally can be saved to disk as a pdf
#' @param x data.frame
#' @param variables vector of variables to be used
#' @param labels 
#' @param xlab 
#' @param save should pdf be saved to disk
#' @param ... additional graphic parameters for barplot (e.g., col)
#' @export
#' @keywords umit
#' @seealso mulithist
#' @return Generates for each variable in a dataframe a barplot, optionally saved to the wd as a pdf
#' @examples 
#' data(cars)
#' multibar(cars,save=F)
multibar <- function(x, variables, labels, xlab = "Antworten", 
	save = TRUE,...) {
	if(class(x)!="data.frame") stop("x is not a Dataframe")
	if(missing(variables)) variables=1:length(x)
	if (!missing(labels)) {
	 if(is.character(labels) & length(variables)==length(labels)) {
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



