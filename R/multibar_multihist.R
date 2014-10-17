
#' @title Multiple separate histograms
#'
#' @description produces a histogram for each variable in a dataframe or its subset, which optionally can be saved to disk as a pdf
#' @param x data.frame
#' @param variables vector of variables to be used, e.g., c(1,4:10)
#' @param labels character vector of labels, useful if elaborate labels are stored
#' in an object separate from the dataframe
#' @param xlab
#' @param save should pdf be saved to disk
#' @param format specify the format in which to save the file (e.g., jpeg, pdf, png, eps)
#' @param ... additional graphic parameters for hist (e.g., densitiy, col, ylab, etc)
#' @export
#' @keywords umit plot
#' @seealso \code{\link{multibar}} \code{\link{hist}}
#' @return none
#' @examples 
#' data(crime_data)
#' multihist(crime_data, save=FALSE, ylab="Freq. #")
#' \dontrun{ 
#' multihist(dd)
#' multibar(dd, 80:82, save = T)
#' multibar(x2,c(1:3))
#' multibar(neo_n,5,ylab="häu")
#'}

multihist <- function(x, variables, labels = TRUE, xlab = "Verteilung",save=TRUE,format=jpeg,...) {
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
			hist(x[[i]], main = ll[i], xlab = ll[i], cex.main = 0.8,...)
				if (save) {
			#dev.copy2pdf(file = paste0(ll[[i]], " (Histogramm).pdf"), height = 8, 				width = 12)
				  dev.copy(format,file = paste0(ll[[i]], " (Histogramm).",substitute(format)))
				  dev.off() 
				}
#dev.copy(device=format, file = paste0(ll[[i]], " (Histogramm).",substitute(format)), height = 8, width = 12)
				}
	}
}





#' @title Multiple separate barplots
#'
#' @description given a dataframe, produces a barplot for each variable or a subset of variables, which optionally can be saved to disk as a pdf
#' it's actually just a wrapper for \code{\link{barplot}}
#' @param x data.frame
#' @param variables vector of variables to be used
#' @param labels labels for each seperate histogram
#' @param xlab general subscript for each histogram
#' @param save should pdf be saved to disk
#' @param ... additional graphic parameters for barplot (e.g., ylab col)
#' @export
#' @keywords umit plot
#' @seealso \code{\link{multihist}} \code{\link{barplot}}
#' @return Generates for each variable in a dataframe a barplot, optionally saved to the wd as a pdf
#' @examples 
#' data(cars)
#' multibar(cars,save=FALSE)
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



