#' Themen für ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' require(ggplot2)
#' ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb()
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
ggplot(tibble(x=rnorm(100),y=rnorm(100)^3),aes(x,y))+geom_point()+stat_smooth()+theme_isb()

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


theme_min <-  function (size=10, font=NA, face='plain', 
                      panelColor=backgroundColor, axisColor='#999999', 
                      gridColor=gridLinesColor, textColor='black') 
{
  theme_text = function(...)
    ggplot2::theme_text(family=font, face=face, colour=textColor, 
                        size=size, ...)
  
  opts(
    axis.text.x = theme_text(),
    axis.text.y = theme_text(),
    axis.line = theme_blank(),
    axis.ticks = theme_segment(colour=axisColor, size=0.25),
    panel.border = theme_rect(colour=backgroundColor),
    legend.background = theme_blank(),
    legend.key = theme_blank(),
    legend.key.size = unit(1.5, 'lines'),
    legend.text = theme_text(hjust=0),
    legend.title = theme_text(hjust=0),
    panel.background = theme_rect(fill=panelColor, colour=NA),
    panel.grid.major = theme_line(colour=gridColor, size=0.33),
    panel.grid.minor = theme_blank(),
    strip.background = theme_rect(fill=NA, colour=NA),
    strip.text.x = theme_text(hjust=0),
    strip.text.y = theme_text(angle=-90),
    plot.title = theme_text(hjust=0),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
}
