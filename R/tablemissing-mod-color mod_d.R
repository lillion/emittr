

# Original function from package vmv
# http://cran.r-project.org/web/packages/vmv/index.html

# Based on a modified version by "Friendly" (Wiki Contributor)
# http://scs.math.yorku.ca/index.php/Visualizing_missing_data

# The minor modifications in this version by David Torres Irribarra
# April 16, 2013

# Additional changes to allow for color parameters by Rebecca Freund
# April 17, 2013

# Additional changes by e.d.
# May, 2014


#' @title Graphische Darstellung unterschiedlicher missing-value Muster über mehrere Items
#'
#' @description Für eine Anzahl von Items werden die jeweilige Anzahl der Missing per Item
#'  als auch die unterschiedlichen Missing-Muster über alle Items graphisch dargestellt
#' @param x mehrere Items als Datensatz (dataframe)
#' @param sortby wonach soll die Darstellung sortiert werden: nach Muster -> "Muster", nach Item -> "Spalte",oder nach beidem "beides" (voreingestellt)
#' @param main Überschrift der Darstellung
#' @param hue Farbe (H-ue), d.h. Grundfarbe, im hsv Farbmodel
#' @param value Wert (V-alue), d.h. Helligkeit, im hsv Farbmodel
#' @param cut Minimale Häufigkeit eines Missing-Musters, damit es in der Graphik mitaufgenommen wird
#' @export
#' @keywords missings
#' @seealso table
#' @return plot
#' @examples 
#' demoframe <- data.frame(sapply(1:15,function(x) rnorm(50,100,10)))
#' demoframe <- as.data.frame(lapply(demoframe, function(x) "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, .2))))))
#' tablemissing_d(demoframe,cut=0) # Muster, die nur 1x vorkommen, werden unterdrückt
#'
tablemissing_d <- function (x, sortby = "beides", main = "Darstellung der Missings", hue = .6, value = .9, cut=0) 
{
    par(xpd=NA)
    x1 <- as.numeric(apply(x, 2, function(x) length(which(is.na(x)))))
    x1 <- c(x1, nrow(x))
    z1 <- ifelse(is.na(x), 0, 1)
    tab = table(apply(z1, 1, paste, collapse = ","))
    tab = tab[order(names(tab), decreasing = TRUE)]
    tab = data.frame(combination = names(tab), count = as.numeric(tab))
    tabp <- t(apply(tab, 1, function(x) {
        as.numeric(unlist(strsplit(x, ",", fixed = TRUE)))
        }))
    tabp <- as.data.frame(tabp)
    tabp <- rbind(tabp, x1)
    names(tabp) <- c(names(x), "Total")
    row.names(tabp) <- c(seq(1, nrow(tab)), "Total")
    if (sortby == "variable") {
        tabfinal <- tabp
    }
    if (sortby == "Muster") {
        tabfinal <- tabp[-nrow(tabp), ]
        tabfinal <- tabfinal[order(tabfinal$Total, decreasing = TRUE), 
        ]
        tabfinal <- rbind(tabfinal, tabp[nrow(tabp), ])
    }
    if (sortby == "Spalte") {
        tabfinal <- tabp[, -ncol(tabp)]
        vals <- unlist(tabfinal[nrow(tabfinal), ])
        tabfinal <- tabfinal[order(vals, decreasing = TRUE)]
        tabfinal <- cbind(tabfinal, Total = tabp$Total)
    }
    if (sortby == "beides") {
        tabf <- tabp[-nrow(tabp), ]
        tabf <- tabf[order(tabf$Total, decreasing = TRUE), ]
        tabf <- rbind(tabf, tabp[nrow(tabp), ])
        tabfinal <- tabf[, -ncol(tabf)]
        vals <- unlist(tabfinal[nrow(tabfinal), ])
        tabfinal <- tabfinal[order(vals, decreasing = TRUE)]
        tabfinal <- cbind(tabfinal, Total = tabf$Total)
    }
 

    finaltable <- tabfinal[tabfinal$Total>cut,]
    finaltable
    par(mar = c(1, 3, 5, 1))
    nop = nrow(finaltable) - 1
    nov = ncol(finaltable) - 1
    width = 100/(nov)
    height = 10
    x1 = 0
    x2 = width
    y1 = 30
    y2 = y1 + height
    pylim = y1 + 10 * nop
 print(pylim)
    plot(10, 20, type = "n", xlim = c(0, 120), ylim = c(0, pylim), 
        axes = FALSE, xlab = "", ylab = "", main = main)
    text(-5, y1+(pylim-y1)/2, "Missing Muster", srt=90, cex=1.25) 
 
if (cut!=0) text(60-10, y1-2.5, paste0("Missing Muster, die nur ",cut," Mal vorkommen, hier nicht angezeigt"), cex=.7) 
 
    for (i in nop:1) {
        for (j in 1:nov) {
            if (finaltable[i, j] == 0) {
                polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), 
                 col = "#FFFFFF", border = "#000000")
                
            }
            else {
                polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), 
                 col = hsv(hue,(min(finaltable[i, nov + 1]/max(finaltable[-c(1,nrow(finaltable)),nov + 1]),1)),value), border = "#000000")
            }
            x1 = x1 + width
            x2 = x2 + width
        }
        x1 = 0
        x2 = width
        y1 = y1 + height
        y2 = y2 + height
    }
    bx1 = width/4
    bx2 = 3 * bx1
    by1 = 5
    by3 = 25
    bsize = 20
    text(-5,15, "Variables", srt=90, cex=1.25)
    for (i in 1:nov) {
        m = finaltable[nop + 1, i]/finaltable[nop + 1, nov + 
        1] * bsize
        p = bsize - m
        by2 = by1 + p
        polygon(c(bx1, bx2, bx2, bx1), c(by1, by1, by2, by2), 
            col = hsv(hue,1,value), border = "#000000")
        Sys.sleep(.05)
        polygon(c(bx1, bx2, bx2, bx1), c(by2, by2, by3, by3), 
            col = "#F0F0F0", border = "#000000")
        legend(bx1, 0, paste0("*",names(finaltable)[i]), bty = "n", xjust = 0, 
            yjust = 0.5, x.intersp = -1, cex = .7)
        text((bx1+bx2)/2, (by1+by3)/2, paste0(finaltable[nop + 1, i],"*"), col="white", cex = .7)
        bx1 = bx1 + width
        bx2 = bx1 + width/2
    }
#polygon(c(0,width*nov,width*nov,0),c(3,3,26,26))
lines(c(0,0,width*nov,width*nov),c(26,5,5,5))
lines(c(-1,1),c(25,25))
text(-3,25,finaltable[nop+1, nov + 1],cex=.7)
lines(c(-1,1),c(5,5))
text(-3,5,0,cex=.7)
    px1 = 110
    py1 = 30+2
    py2 = py1 + 6
cat("hello",(tabfinal$Total))
    text(px1+5, pylim + 10, paste0("# Fälle: (von ", tail(tabfinal$Total,1),")"), adj=c(1,0.5))

    for (i in nop:2) {
        if (sum(finaltable[1, 1:nov]) == nov) {
            psize = finaltable[i, nov + 1]/(finaltable[nop + 
                1, nov + 1] - finaltable[1, nov + 1]) * 20
        }
        else {
            psize = finaltable[i, nov + 1]/(finaltable[nop + 
                1, nov + 1]) * 20
        }
        if (psize < 0.2) {
            psize = 0.2
        }
        px2 = px1 + psize
        polygon(c(px1, px2, px2, px1), c(py1, py1, py2, py2), 
            col = hsv(hue,(min(finaltable[i, nov + 1]/max(finaltable[-c(1,nrow(finaltable)),nov + 1]),1)),value), border = "#000000")

        text(px1-2, (py1+py2)/2, finaltable[i, nov + 1], adj=c(1,0.5), cex = 0.7)
        py1 = py1 + 10
        py2 = py2 + 10
    }
    psize = finaltable[1, nov + 1]/finaltable[nop + 1, nov + 1] * 20
    px2 = px1 + psize
    if (sum(finaltable[1, 1:nov]) == nov) { 
        polygon(c(px1, px2, px2, px1), c(py1, py1, py2, py2), 
            col = hsv(hue,1,value), border = "#000000")
        } else {
            polygon(c(px1, px2, px2, px1), c(py1, py1, py2, py2), 
                col = "#636363", border = "#000000")
        }

        text(px1-2, (py1+py2)/2, finaltable[1, nov + 1], adj=c(1,0.5), cex = 0.7)
        #text(px1-2, (by1+by3)/2, finaltable[nop+1, nov + 1], adj=c(1,0.5), cex = 0.7)
 
 text(px1-4, (by1+by3-4)/2, paste0("* Anzahl\nmissings (auf ",finaltable[nop+1, nov + 1],")\npro Var"),srt=0, cex = 200/par("usr")[4]+.02) 

        legend(30, par("usr")[4]*1.07 , legend = c('vorhanden','fehlend')
            , fill = c(hsv(hue,1,value),"#FFFFFF"), border = "#000000"
            , bty = 'n', ncol = 2) 

        par(xpd=FALSE)
        finaltable
}