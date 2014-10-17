#' @title Extrahiert automatisch Skalen aus einem EFA Objekt
#'
#' @description given the pattern maxtrix of an EFA with more than one factor, combine items loading on one particular factor to a scale and save it in an object. Items with unclear loading pattern (i.e. non trivial loadings on more than one factor) can be either evaluated using a loading difference to the next highest loading (parameter loadingdifference) or using the Fürntratt criterion. If an item fails to meet the selected criterion it will not be included in the scale.
#' @param faobject Objekt aus einer \link{fa} o.ä.
#' @param ladungsunterschied Nebenladungsunterschied, der noch akzeptabel ist für das Beibehalten eines Items
#' @param datensatz Datensatz (data.frame), der die Items enthält
#' @param name Name (Präfix) der Skalen
#' @param fuerntratt (logisch) Soll das Fürntratt-Kriterium bei der Faktorzuweiseung angewandt werden
#' @export
#' @keywords fa
#' @seealso \link[psych]{fa}, \link{factanal}
#' @return list of scales
#' @examples \dontrun{
#'
#'}
fa.skalenbilden <- function(faobject,ladungsunterschied=.10,datensatz=NULL, name="skala",fuerntratt=FALSE){
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
        if(sort(abs(ladungen[i, ]),decreasing=TRUE)[1]-sort(abs(ladungen[i, ]),decreasing=TRUE)[2]<.10) ad <- NA else ad <- which.max(abs(ladungen[i, ]))
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
