#pth <- "~/Dropbox/R Ideen/"
 
#fls <- file.path(pth, dir(pth))
 
FUN <- function(x) {
    cont <- readLines(x)
    cont[grepl("#' @S3", cont)] <- "#' @export"
    cont[length(cont) + 1] <- ""
    cat(paste(cont, collapse="\n"))
}
 
#lapply(fls, FUN)



