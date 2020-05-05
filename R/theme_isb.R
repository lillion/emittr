#' Helles Thema für ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' require(ggplot2)
#' p1 <- ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb()
theme_isb <- function () { 
  theme_bw(base_size=12, base_family="Aristotelica Text") %+replace% 
    theme(
      panel.background  = element_blank(),
      legend.background = element_rect(fill="transparent", colour=NA),
      panel.border = element_blank(),
      panel.grid.major.x =element_blank(),
      panel.grid.major.y = element_line(colour = "grey"),
      #panel.grid.minor = element_line(colour = "grey"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "grey23"),
      axis.ticks = element_line(colour = "grey"),
      axis.ticks.length=unit(8, "pt"),
      #axis.text = element_text(face = 'italic'),
      axis.title = element_text(face = 'bold'),
      legend.key = element_rect(fill="transparent", colour=NA)
      
    )
}


#' Themen für ggplot2
#'
#' @return
#' @export
#'
#' @rdname theme_isb
#' @examples
#' p2 <- ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb_grey()
theme_isb_grey <- function(){
  theme_classic() %+replace% 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color="grey",size=.3),
          panel.grid.minor.y = element_line(color="lightgrey",size=.1),
          axis.line = element_line(arrow=arrow(angle = 30, length = unit(5, "points"),
                                               ends = "last", type = "closed"),
                                   color="darkgrey",size=.3),
          axis.ticks = element_line(color="darkgrey"),
          axis.text.x = element_text(angle=0,hjust=0.5))
}

#' ggplot2 theme ohne Pfeile an den Achsen
#'
#' @param Winkel 
#' @param hVer 
#'
#' @return
#' @export
#'
#' @examples
#' require(ggplot2)
#' p4 <- ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb_oPfeil()
#' p5 <- ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb_oPfeil_quer()
#' cowplot::plot_grid(p4, p5, labels = c("theme_isb_oPfeil","theme_isb_oPfeil_quer"))
theme_isb_oPfeil <- function(Winkel=90, hVer=1){
  theme_classic() %+replace% 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color="grey",size=.3),
          panel.grid.minor.y = element_line(color="lightgrey",size=.1),
          axis.line = element_line(color="darkgrey",size=.3),
          axis.ticks = element_line(color="darkgrey"),
          axis.text.x = element_text(angle=Winkel,hjust=hVer),
          axis.title = element_text(face = 'bold'))
}


#' Title
#'
#' @param Winkel 
#' @param hVer 
#'
#' @rdname theme_isb_oPfeil
#' @return
#' @export
#'
#' @examples
#' ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+stat_smooth(method="lm",color="red",fill="lightblue")+theme_isb_oPfeil_quer()
theme_isb_oPfeil_quer <- function(Winkel=0, hVer=.5){
  theme_classic() %+replace% 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color="grey",size=.3),
          panel.grid.minor.x = element_line(color="lightgrey",size=.1),
          axis.line = element_line(color="darkgrey",size=.3),
          axis.ticks = element_line(color="darkgrey"),
          axis.text.x = element_text(angle=Winkel,hjust=hVer),
          axis.title = element_text(face = 'bold'))
}


#' Thema ohne Achsen
#'
#' @return
#' @export
#'
#' @rdname theme_isb
#' @examples
#' p3 <- ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb_ohneAxis()
#' cowplot::plot_grid(p1, p2, p3, labels = c("theme_isb","theme_isb_grey","theme_isb_ohneAxis"),hjust=-.1,scale=.85,label_size=12)
theme_isb_ohneAxis <- function(){
  theme_classic() %+replace% 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color="grey",size=.3),
          panel.grid.minor.y = element_line(color="lightgrey",size=.1),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle=45,hjust=1))
}


#' Title
#'
#' @param size 
#' @param font 
#' @param face 
#' @param backgroundColor 
#' @param panelColor 
#' @param axisColor 
#' @param gridColor 
#' @param textColor 
#'
#' @return
#' @export
#' @rdname theme_isb
#'
#' @examples
theme_min <-  function (size=10, font=NA, face='plain', backgroundColor="white",
                        panelColor="grey", axisColor='#999999', 
                        gridColor="grey", textColor='black') 
{
  theme_classic(base_size = size,base_family =font) %+replace% 
    theme(
      axis.text.x = element_text(),
      axis.text.y = element_text(),
      axis.line = element_blank(),
      axis.ticks = element_line(colour=axisColor, size=0.25),
      panel.border = element_rect(fill=NA,colour=NA),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(1.5, 'lines'),
      legend.text = element_text(hjust=0),
      legend.title = element_text(hjust=0),
      panel.background = element_rect(fill=backgroundColor, colour=NA),
      panel.grid.major = element_line(colour=gridColor, size=0.25),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill=NA, colour=NA),
      strip.text.x = element_text(hjust=0),
      strip.text.y = element_text(angle=-90),
      plot.title = element_text(hjust=0),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
}