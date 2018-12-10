#' Schnelle Darstellung von Data.frames als HTML
#'
#' @description Mit dieser einfachen Funktion können Ergebnisse und Daten, die als data.frame vorliegen, 
#' als HTML dargestellt und so einfach in Word kopiert werden.
#' @param dataframe 
#'
#' @return HTML
#' @export
#'
#' @examples
#' data("crime_data")
#' describe(crime_data) %>% print(digits=2) %>% htmlschnell()
#' # einen Datensatz direkt
#' htmlschnell(crime_data[1:10,])
#' # eine Regression
#' lm_coef_spss_d(fit,stellen = 2) %>% htmlschnell()

htmlschnell <- function(dataframe) {
  mymyb <- function(x){
    paste("<tr>",x,"</tr>")  
  }
  
  mymy <- function(x){
    paste0("<td>",x,"</td>", collapse="")  
  }
  
  ifelse(is.data.frame(dataframe),x <- dataframe,stop("kein data.frame!"))
  if(!is.null(rownames(x))) {
    x$Variablen <-rownames(x)
    x <- x[c(dim(x)[2],1:dim(x)[2]-1)]
  }
  if(!is.null(colnames(x))) row1 <-colnames(x)
  row1 <- paste0('<th class="thead firstcolborder">',row1,"</th>", collapse="")  
  row1 <- mymyb(row1)
  
  
  rowlast <- x[dim(x)[1],]
  rowlast <- paste0('<th class="tdata horline lasttablerow">',rowlast,"</th>", collapse="") 
  rowlast <- mymyb(rowlast)
  
  x <- x[-dim(x)[1],]
  
  x <- t(x) %>% as.data.frame()
  xx <-sapply(x,mymy)
  xx <- mymyb(xx)  
  # xx <- paste("<tr>",xx,"</tr>")  
  

  
  
  cat("<html>
 <style>
html, body { background-color: white; }
table { border-collapse:collapse; border:none; padding:0.2cm;}
caption { font-weight: bold; text-align:left; }
.thead { border-top:double; text-align:center; font-style:italic; font-weight:normal; }
.tdata { text-align:left; font-style:normal; font-weight:normal;}
.secondtablerow { border-bottom:1px solid; text-align:center; }
.leftalign { text-align:left; vertical-align:middle; }
.centeralign { text-align:center; }
.lasttablerow {  border-bottom:double; }
.totcol {  }
.tothi { font-weight:bolder; font-style:italic; }
.td_n { color:black }
.td_c { color:#993333 }
.td_rw { color:#333399 }
.td_cl { color:#339933 }
.td_ex { color:#339999 }
.summary { text-align:right; font-size:0.9em; font-style:italic; }
.horline {  }
.firstcolborder { border-bottom:1px solid; }
</style> 
<body>

 <table>",row1, xx,rowlast,"</table>
      </body></html>",file = "test.html")
  utils::browseURL("test.html")
  # rstudioapi::viewer("test.html")
}
