
#' @title <brief desc>
#'
#' @description <full description>
#' @param faobject saved fa object
#' @param ladungsunterschied should loadings be assigned to a factor by loading difference
#' @param datensatz data.frame containing the items
#' @param name Name the objects to be saved
#' @param fuerntratt should loading be assigned to factor according to Fuerntratt 
#' @export
#' @keywords fa
#' @seealso fa
#' @return list of scales
#' @examples \dontrun{
#'
#'}
buildscale <- function(faobject, # saved fa object
  ladungsunterschied=.10, # should loadings be assigned to a factor by loading difference
  datensatz=NULL, # data.frame containing the items
  name="skala", # Name the objects to be saved 
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
  a <- lapply(a, function(x) datensatz[x])
  for (i in 1:length(a)){
    assign (paste0(name,i),a[[i]],envir=globalenv())
  }
  
  return(a)
}
