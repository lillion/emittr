#' @title Extract scales from an EFA object
#'
#' @description given the pattern maxtrix of an EFA with more than one factor, combine items loading on one particular factor to a scale and save it in an object. Items with unclear loading pattern (i.e. non trivial loadings on more than one factor) can be either evaluated using a loading difference to the next highest loading (parameter loadingdifference) or using the Fürntratt criterion. If an item fails to meet the selected criterion it will not be included in the scale.
#' @param faobject saved fa object
#' @param ladungsunterschied should loadings be assigned to a factor by loading difference
#' @param datensatz data.frame containing the items
#' @param name Name the objects to be saved
#' @param fuerntratt should loading be assigned to factor according to Fuerntratt 
#' @export
#' @keywords fa
#' @seealso \code{\link{fa}}
#' @return either the itemnumbers per scale or objects of the scales themselves saved to the environment
#' @examples 
#' if(!require(psych)) {
#' efa <- fa(pers_data[-c(6,11,16)],nfactors = 3)
#' fa.buildscale(efa) # all 3 scales are correctly extracted, no items is discarded
#' fa.buildscale(efa,datensatz=pers_data[-c(6,11,16)]) # this saves scales to workspace
#' }
fa.buildscale <- function(faobject, # saved fa object
  ladungsunterschied=.10, # should loadings be assigned to a factor by loading difference
  datensatz=NULL, # data.frame containing the items
  name="scale", # Name the objects to be saved 
  fuerntratt=FALSE # should loading be assigned to factor according to Fuerntratt
  ){
  class(faobject$loadings) <- "matrix"
  ladungen <- faobject$loadings
  
  factorindex <- c()
  
  if(fuerntratt) {
    h2 <- faobject$communality
    for (i in 1:nrow(ladungen)) {
      if((max(abs(ladungen[i, ])))^2/h2[i]<.50) ad <- NA else ad <- which.max(abs(ladungen[i, ]))
      factorindex <- c(factorindex, ad)
      
      
    }} else {
      for (i in 1:nrow(ladungen)) {
        if(sort(abs(ladungen[i, ]),decreasing=T)[1]-sort(abs(ladungen[i, ]),decreasing=T)[2]<.10) ad <- NA else ad <- which.max(abs(ladungen[i, ]))
        factorindex <- c(factorindex, ad)
      }
    }
  
  a <- vector("list", length(table(factorindex)))
  
  for (i in 1:length(table(factorindex))){
    a[[i]] <-which(factorindex==i)
  }    		
  
  #return(table(factorindex,useNA="always"))
  
  if (is.null(datensatz) || class(datensatz)!="data.frame") {
    a <-  list(a, nofactor=which(is.na(factorindex)))
    return(a)
  }
  b <- lapply(a, function(x) datensatz[x])
  for (i in 1:length(b)){
    assign (paste0(name,i),b[[i]],envir=globalenv())
  }
  cat("\nScales saved to your workspace!\n")
  return(list(a, nofactor=which(is.na(factorindex))))
}
