#' @title Hierarchische lineare Regression mit mehreren Bl\code{ö}cken
#'
#' @description Hierarchichal regression with several blocks estimated in sequence
#' This is basically a wrapper function for \code{\link{lm}} that splits the formula 
#' into user denoted blocks and computes its models in order to evaluate the incremental validity of each predictor set.
#' @param formula the complete formula including all blocks from the hierarchical regression
#' @param blocks numeric vector denoting the length of each block e.g. c(2, 3, 1) meaning 2 predictors then 3 predictors then 1 predictor 
#' @param data the dataframe holding the variables for the estimation of the lm
#' @param summary should the results be summaries of the lm object
#' @param ... additional parameters for \code{\link{lm}} 
#' @export
#' @keywords lm regression hierarchical regression
#' @seealso \code{\link{lm}}
#' @return list of the regression models
#' @examples \dontrun{
#' # hierarchical regression with 2 blocks, 1:sex, 2: age + school
#' lm_hierarch(conscien ~ sex + age + school, c(1),data=neoffi)
#'}
#' # 4 blocks with each time one additional predictor
#' lm_hierarch_d(formula=mpg ~ disp + hp + wt + drat, blocks=c(1,1,1,1), summary=TRUE, data=mtcars)
#' # 2 blocks with each time 2 additional predictors 
#' lm_hierarch_d(mpg ~ disp + hp + wt + drat, c(2,2), data=mtcars) 
#'
lm_hierarch_d <- function(formula, # the complete formula, including all blocks from the hierarchical regression
 blocks, # numeric vector denoting the lenght of each block, e.g. c(2,3,1)
  data, # the dataframe holding the variables for the estimation of the lm
  summary = FALSE,
  ...) # additional parameters for {code\link{lm}}
{
	if (!is.numeric(blocks)) stop
	txt <- deparse(formula)
	#print(txt)
	limits <- c(gregexpr("\\+",txt,fixed=FALSE)[[1]]-1,nchar(txt))
		if (length(limits)>sum(blocks)) blocks <- c(blocks,length(limits)-sum(blocks))
	cat("\n",length(blocks), "Blöcke mit insgesamt",paste(cumsum(blocks),collapse=", "),"Prädiktoren\n\n")
	blocks <- limits[cumsum(blocks)]
	#print(limits)
	if (summary) 	results <- lapply(blocks, function(x) summary(lm(as.formula(strtrim(txt,x)),data=data)))
		else results <- lapply(blocks, function(x) lm(as.formula(strtrim(txt,x)),data=data))
	names(results) <- sapply(blocks, function(x) strtrim(txt,x))
	return (results)	
	
}




