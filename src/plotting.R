plotTimeSeries <- function(data, title) {
    plot <- ggplot(data, aes(x=Date, y=Open), size = 0.1) +
        theme_gray(base_family = "Gill Sans") +
        ggtitle(title) +
        # xlab("Date") +
        geom_line() +
        # geom_point(size = 0.1, fill = "white") +
        
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position    = "none",
            legend.text        = element_text(size=11),
            legend.title       = element_blank(),
            axis.title.y       = element_blank(),
            axis.line          = element_line(colour = "white"),
            axis.line.x        = element_line(colour = "black"),
            axis.text          = element_text(hjust=0.5, size=11, colour="black"),
            axis.title         = element_text(hjust=0.5, size=11),
            plot.title         = element_text(hjust=0, size=12)
        )
    # scale_colour_manual(values=colorPalette) +
    # scale_shape_manual (values=shapePalette) +
    # scale_size_manual  (values=shapeSizes)
    
    return(plot)
}


plotHistoricalData <- function(historicalData) {
    plot <- plotTimeSeries(historicalData@data, historicalData@name);
    return(plot);
}