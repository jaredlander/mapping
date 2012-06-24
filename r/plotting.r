# mapping function
#' map.plot
#' 
#' Plot Maps
#' 
#' Takes a \code{\link{data.frame}} containing shapefile data and plots it along with some measure.
#' 
#' Will probably need to make changes for when plotting continuous data.
#' 
#' @author Jared P. Lander
#' @aliases map.plot
#' @export map.plot
#' @import ggplot2
#' @param data \code{\link{data.frame}} resulting from using \code{\link{fortify}} on a shapefile object.
#' @param variable Character indicating which column should be plotted.
#' @param fill.color.high Color to use for top of gradient.
#' @param space Color space to use for gradient.
#' @param path.color Color to use for shapefile lines.
#' @param title Title of plot.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param barheight Height of legend bar.
#' @param formatter How the legend numbers should be formatted.  Takes a function.
#' @param legend.position Position of legend.
#' @return A ggplot object.
#' @examples
#' 
#' \dontrun{
#' nyc.income <- join(nyc.df, income2010, by="Key")
#' mana <- nyc.income[nyc.income$County == 61, ]
#' map.plot(mana, "Median.Income", formatter=multiple_format(extra=dollar, multiple="K"))
#' }
#' 
map.plot <- function(data, variable, fill.color.high=muted("green"), space="Lab", path.color="white", title=NULL, 
                     xlab=NULL, ylab=NULL, barheight=15, formatter=multiple_format(multiple="K", extra=comma),
                     legend.position=c("right", "bottom", "left", "top", "none"))
{
    # generate a bunch of prebuilt map options for easier plotting
    mapOpts <- map.options()
    
    # get legend.position
    legend.position <- match.arg(legend.position)
    
    # start plotting
    p <- ggplot(data) + 
        # lat/long and group
        aes(x=long, y=lat, group=group) + 
        # coloring based on the variable
        aes_string(fill=variable) +  geom_polygon() + 
        geom_path(color=path.color) + coord_equal() + 
        # mapping options
        mapOpts + 
        # x/y labels
        labs(x=xlab, y=ylab) + 
        # make the legend be titleless and have a long, tal color bar
        guides(fill=guide_colorbar(title=NULL, ticks=FALSE, barheight=barheight)) + 
        # make the color scale a gradient and use the chosen formatter
        scale_fill_gradient2(labels=formatter, space=space, high=fill.color.high) + 
        # title of plot
        opts(title=title)
    
    return(p)
}

#' map.options
#' 
#' Common mapping options
#' 
#' This builds a ggplot opts list that is commonly used for plotting.  Currently it only gives the default, perhaps I'll make it so users can pick and choose.
#' 
#' @author Jared P. Lander
#' @aliases map.options
#' @export map.options
#' @import ggplot2
#' @return A \code{\link{list}} of ggplot items.
map.options <- function()
{
    list(opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank(), panel.background=theme_blank()))
}