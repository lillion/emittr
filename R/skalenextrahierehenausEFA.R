
skalenbilden2 <- function(faobject,ladungsunterschied=.10,datensatz=NULL, name="skala",fuerntratt=F){
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
