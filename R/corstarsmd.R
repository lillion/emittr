#' @title Correlation table with significance stars
#'
#' @description provides a table of a correlation matrix including denoted significance 
#' @param x dataframe object
#' @param type options are none, html or markdown (hands table to kable)
#' @param digits how many digits should be displayed
#' @param tenperc (logical) should p<.10 also be denoted by a bubble (°)
#' @param abbrev (logical) should columnames be shortened
#' @param diagonal (logical) should the diagonal be filled
#' @param lower (logical) should the lower triangle be used
#' @param ... additional parameters for kable
#' @export
#' @importFrom psych corr.test
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

corstarsmd <- function(x, type="markdown", digits=3, tenperc=FALSE, abbrev=TRUE, diagonal=FALSE, lower=TRUE, ...){ 
  require(knitr)
  x <- x[sapply(x,is.numeric)]
  x <- as.matrix(x) 
  R <- psych::corr.test(x)$r # psych
  p <- psych::corr.test(x)$p #psych
  if (tenperc){
#     mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .1, "✝  ", "   "))))
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", ifelse(p < .1, "°  ", "   "))))
  } else {
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")) ) }
  R <- format(round(cbind(rep(-1.111, ncol(x)), R), digits ))[,-1] 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), "   ", sep="") 
  # rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), " ", sep="") 
  if (abbrev) colnames(Rnew) <- abbreviate(colnames(Rnew), minlength = digits + 3)
  Rnew <- as.data.frame(Rnew) 
#if (!diagonal) Rnew <- Rnew[,-length(Rnew[1,])]
  if (nrow(Rnew) == ncol(Rnew)) {
    Rnew <- sapply(Rnew, as.character)
    if(lower) Rnew[!lower.tri(Rnew, diag = diagonal)] <- " " else Rnew[!upper.tri(Rnew, diag = diagonal)] <- " " 
    Rnew <- as.data.frame(Rnew) 
  }
rownames(Rnew) <- colnames(x) 
  if(lower) Rnew <- Rnew[, -length(Rnew[1, ])] else Rnew <- Rnew[-length(Rnew[,1 ]), ]
  if(type=="none") return(Rnew) 
  return(kable(Rnew,format=type,...)) 
}