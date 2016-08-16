#' Beschriftung zu Graphik hinzufügen
#'
#' @param ... der Inhalt der Legende, meist legend=c(), dazu die Position der Legende ("right", "left", "top", "bottom" inkl. Kombinationen)
#' @param grid logisch: soll ein Gitter gezeichnet werden
#' @param intervall Abstand der Gitterlinien
#' @param lty Art der Gitterlinien (voreingestellt 3) 
#'
#' @return Legend
#' @export
#'
#' @examples
#' add_legend("right", legend=c("Lotta", "Marie", "Olivia"), lty=c(1:3), col=c("steelblue", "indianred", "pink"), horiz=F, bty='n', cex=0.9) # hier die Beschriftung
add_legend <- function(..., grid=TRUE, intervall=.5, lty=3) {
  linien_x <- par("xaxp")
  linien_y <- par("yaxp")
  if(grid) abline(v=seq(linien_x[1],linien_x[2], intervall),h=seq(linien_y[1],linien_y[2], intervall), col="lightgrey",lty=3)
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0.1), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}





#' Plot Funktion mit einigen Voreinstellungen zur leichteren Handhabung
#'
#' @param ... 
#' @param position 
#' @param bty 
#' @param las 
#' @param cex.axis 
#' @param Schrift 
#'
#' @return
#' @export
#'
#' @examples
#' plotum(23:27,4:8,Schrift="Gill Sans")
#' add_legend("right", legend=c('Text hier', 'Text2 hier', '...'), lty=c(1:3), col=c('pink','indianred', 'steelblue'), horiz=FALSE, bty='n', cex=0.9)
plotum <- function(..., position="right", bty='n', las=1,cex.axis=.8, Schrift="Garamond")
  {
  par(mar = c(4, 4, 1.4, 5))
  plot(..., bty=bty, las=las, cex.axis=cex.axis)
  cat("die folgende Zeile kann verwendet werden, um eine Legende zu erstellen!\n")
    cat("add_legend(\"",position,"\", legend=c('Text hier', 'Text2 hier', '...'), lty=c(1:3), col=c('pink','indianred', 'steelblue'), horiz=FALSE, bty='n', cex=0.9)",sep = "")
}
