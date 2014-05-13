#' @title given a lavaan multigroup object returns a dataframe to be used in publication
#'
#' @description <full description>
#' @param inva a lavaan object of an invariance test
#' @export
#' @keywords lavaan, invariance
#' @seealso lavaan
#' @return dataframe
#' @examples \dontrun{
#' library(semTools)
#' inva <- measurementInvariance(model_reak,laurasdaten,group="Geschlecht",estimator="WLSMV")
#' extractlavaan2table(inva)
#' gsub(" 0.000", " <.001",print(xtable(extractlavaan2table(inva),digits=3), type="latex", sanitize.text.function=identity),useBytes=T)
#'}
extractlavaan2table <- function(inva=result){

df <- data.frame(t(fitMeasures(inva[[1]])[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower','rmsea.ci.upper','srmr')]))
df <- rbind(df,t(fitMeasures(inva[[2]])[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower','rmsea.ci.upper','srmr')]))
df <- rbind(df,t(fitMeasures(inva[[3]])[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower','rmsea.ci.upper','srmr')]))
df <- rbind(df,t(fitMeasures(inva[[4]])[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower','rmsea.ci.upper','srmr')]))
#df <- rbind(df,t(fitMeasures(inva[[5]])[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'rmsea.ci.lower','rmsea.ci.upper','srmr')]))

rownames(df) <- c("Model 1: configural","Model 2: loadings","Model 3: intercepts","Model 4: means")

df <- data.frame(df[1:4],"DeltaCFI"=rep("-",4),df[5:8],Estimator=rep(inva$fit.configural@Options$estimator,4))

for (i in 4:2) {
	
	df[i,5] <- round(fitMeasures(inva[[i-1]])["cfi"] - fitMeasures(inva[[i]])["cfi"],3)
}
names(df) <- c("<i>χ</i><sup>2</sup>","<i>df</i>",'<i>p</i>', 'CFI', 'ΔCFI','RMSEA', 'CI LO','CI HI','SRMR', 'Estimator')
return(df)	
}

# fitMeasures(inva[[3]])


# inva[[4]]

