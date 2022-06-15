plot_hist_perc <- function ( x, plot_this="percent.mt", ccc="#6e9be5" , cut_here = 15, msg = "Cells with MT >", binwidth= 5 , y=.5) {
  
   
 x$x1 = x[[plot_this]]
  
 ggg= ggplot(x, aes(x=x1)) + 
    geom_histogram(aes(y=(..count..)/sum(..count..)), colour='black' , fill=ccc, alpha=.2 , binwidth= binwidth )+
    # geom_density(alpha=0, fill="grey") +
    theme(legend.position="none",  legend.key = element_blank(),
          # element_blank()
          axis.text.y = element_text(size= 25 ),
          axis.text.x =  element_text(size= 25 ),
          axis.title.x = element_text(size=25),
          axis.title.y     = element_text(size=25), 
          legend.text      =element_text(size=25),
          legend.title = element_text(size=25),
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5)
          # hjust centers the title
    ) + theme(panel.grid.major = element_blank()
              , panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              , axis.line = element_line(colour = "black") # plot border
    ) + ylab ( "percent") + 
   geom_vline(xintercept = cut_here, linetype = 2, color = "red") +
   annotate(geom = "text", x = cut_here , y = y, label = paste0 ( msg , cut_here ), color = "red",
            angle = 90, vjust=2.5) + xlab (plot_this)
  
  
  return ( ggg )
  
  
}


