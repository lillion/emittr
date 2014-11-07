#' @title Correlation table with significance stars
#'
#' @description provides a table of a correlation matrix including denoted significance 
#' @param x dataframe object
#' @param type options are none, html or markdown (hands table to kable)
#' @param digits how many digits should be displayed
#' @param tenperc (logical) should p<.10 also be denoted by a cross
#' @param abbrev (logical) should columnames be shortened
#' @param diagonal (logical) should the diagonal be filled
#' @export
#' @importFrom Hmisc rcorr
#' @keywords correlation semi
#' @seealso \link{cor} 
#' @return text table
#' @examples 
#' data(pers_data)
#' corstarsmd(pers_data[c(7:10,12:15)],type="none", digits=2, tenperc=TRUE)
#' \dontrun{
#' # in order to transfer the output into e.g. Microsoft Word, use the following workflow:
#' for_word <- corstarsmd(pers_data,type = "none",digits=2)
#' library(xlsx)
#' write.xlsx(for_word, "test.xlsx")
#' # the saved file can be opened in Excel, the table copied and pasted into Word
#' }

corstarsmd <- function(x, type="markdown", digits=3, tenperc=FALSE, abbrev=TRUE, diagonal=FALSE,  ...){ 
  #require(Hmisc) 
  require(knitr)
  x <- as.matrix(x) 
  R <- Hmisc::rcorr(x)$r 
  p <- Hmisc::rcorr(x)$P 
  if (tenperc){
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .2, "✝  ", "   "))))
  } else {
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", "   ")) ) }
  R <- format(round(cbind(rep(-1.111, ncol(x)), R), digits ))[,-1] 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), "   ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), " ", sep="") 
  if (abbrev) colnames(Rnew) <- abbreviate(colnames(Rnew), minlength = digits + 3)
  Rnew <- as.data.frame(Rnew) 
  if (nrow(Rnew) == ncol(Rnew)) {
    Rnew[!lower.tri(Rnew, diag = diagonal)] <- ""
  }
  if(type=="none") return(Rnew) 
  return(kable(Rnew,format=type,...)) 
}