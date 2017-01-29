#
#' @title  semipartial, partial und Korrelationen nullter Ordnung eines Regressionsmodell
#'
#' @description berechnet semipartial, partial und Korrelationen nullter Ordnung der Prädiktoren einer linearen Regression mit dem Kriterium
#' @param fit ein Objekt einer \code{\link{lm}} Berechnung
#' @export
#' @keywords "linear model" lm regression korrelation
#' @seealso \code{\link{lm},\link{cor}}
#' @return Datensatz (dataframe) von r, r.,r..
#' @examples
#' library(car)
#' fit <- lm(mpg ~ disp + hp + wt + drat, data=mtcars)
#' cor_lm_d(fit)
cor_lm_d2 <- function(fit)  {
if(class(fit)!="lm") stop(substitute(fit), " ist kein LM Objekt")
dv <- names(fit$model)[1]
dv_data <- fit$model[, dv]  # fit$model[, "crime"]
#ivs <- names(fit$model)[-1]
ivs <- setdiff(all.vars(formula(fit)),dv)
iv_data <- fit$model[, ivs] #  oder: fit$model[ivs]
t <- terms(fit)
tl <- attr(t, "term.labels")
# tmdc <- attr(t, "dataClasses")
# isNumeric <- names(tmdc)[which(tmdc %in% c("numeric"))]
location <- grepl(":", tl)
if(sum(location) >1) stop("Funktion nur für eine Moderation")
interactTerms <- tl[location]
nc <- unique(unlist(strsplit(interactTerms, ":")))
if(length(nc)) {
if(mean(iv_data[,nc[1]],na.rm = T)!=0 | mean(iv_data[,nc[1]],,na.rm = T)!=0) {
cat("\n Zentrierung der Moderatoren\n")
m1 <- iv_data[,nc[1]]-mean(iv_data[,nc[1]],na.rm = T)
m2 <- iv_data[,nc[2]]-mean(iv_data[,nc[2]],na.rm = T)
iv_data$interactTerms <- m1*m2
} else {
iv_data$interactTerms <- iv_data[,nc[1]]*iv_data[,nc[2]]
}
ivs <- c(ivs, "interactTerms")}
# if (length(nc)!=0) {iv_data <- cbind(iv_data,interactTerms=interact)
# ivs <- c(ivs, "interactTerms")}
if (length(ivs)==1) return (data.frame("nullter Ordnung"=cor(iv_data,dv_data),row.names=ivs)) # check if just one predictor
x <- cbind(dv_data,iv_data)
names(x)[1] <- dv
x_omit <- lapply(ivs, function(X) x[, c(dv, setdiff(ivs, X))])
#  lapply(x_omit, head)
fits_omit <- lapply(x_omit, function(X) lm(as.formula(paste(dv, "~ .")), data = X))
resid_omit <- sapply(fits_omit, resid)
iv_omit <- lapply(ivs, function(X) lm(as.formula(paste(X, "~ .")), data = iv_data))
resid_iv_omit <- sapply(iv_omit, resid)
results <- sapply(seq(ivs), function(i) c(nullterOrdnung = cor(iv_data[, i], dv_data),
partial = cor(resid_iv_omit[, i], resid_omit[, i]),
semipartial = cor(resid_iv_omit[,i], dv_data)))
results <- data.frame(t(results))
rownames(results) <- ivs
names(results)[1] <- "nullter Ordnung"
results
}
