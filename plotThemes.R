theme16 <- theme(
    axis.title = element_text( size=12, family="sans") ,
    axis.text = element_text( size=10, family="sans", colour="black") ,
    panel.grid.major.y = element_line(colour="burlywood3", size=.3) ,
    panel.grid.minor.y = element_line(colour="burlywood3", size=.2) ,
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    panel.background = element_rect( fill = "beige", colour = "white" ),    
    axis.line = element_blank() ,
        strip.background = element_blank(),
        strip.text = element_text(size=12, family="sans", face="bold"),
    legend.title = element_text(size=12, family="sans"),
    legend.text = element_text(size=12, family="sans"),
    legend.position = "top"
  )
