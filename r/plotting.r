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
#' @import ggplot2 useful
#' @param data \code{\link{data.frame}} resulting from using \code{\link{fortify}} on a shapefile object.
#' @param variable Character indicating which column should be plotted.
#' @param longitude Character indicating which column stores longitude values.
#' @param latitude Character indicating which column stores latitude values.
#' @param fill.color.low Color to use for bottom of gradient.
#' @param fill.color.high Color to use for top of gradient.
#' @param space Color space to use for gradient.
#' @param path.color Color to use for shapefile lines.
#' @param title Title of plot.
#' @param title.size Size of \code{Title} font.
#' @param title.hjust Horizontal adjustment of \code{Title} position.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param barheight Height of legend bar.
#' @param formatter How the legend numbers should be formatted.  Takes a function.
#' @param legend.position Position of legend.
#' @param lhs Left side of formula to use for facetting.
#' @param rhs Right side of formula to use for facetting.
#' @param facet The type of facetting, if any, to use.
#' @param wrap.nrow The number of rows to to use when using facet_wrap.
#' @param wrap.ncol The number of columns to to use when using facet_wrap.
#' @param scales The scales to be employed when faceting.
#' @return A ggplot object.
#' @examples
#' 
#' \dontrun{
#' nyc.income <- join(nyc.df, income2010, by="Key")
#' mana <- nyc.income[nyc.income$County == 61, ]
#' map.plot(mana, "Median.Income", formatter=multiple_format(extra=dollar, multiple="K"))
#' }
#' 
map.plot <- function(data, variable, longitude="long", latitude="lat", 
                     fill.color.low=muted("green"),  fill.color.high=muted("green"), space="Lab", 
                     path.color="white", 
                     title=NULL, title.size=15, title.hjust=.5,
                     xlab=NULL, ylab=NULL, barheight=15, formatter=percent,
                     legend.position=c("right", "bottom", "left", "top", "none"), 
                     lhs=NULL, rhs=NULL, facet=c("none", "facet_wrap", "facet_grid"), wrap.nrow=NULL, wrap.ncol=NULL, 
                     scales=c("fixed", "free", "free_y", "free_x")
                     )
{
    # old formatter multiple_format(multiple="K", extra=comma)
    # generate a bunch of prebuilt map options for easier plotting
    mapOpts <- map.options()
    
    # get legend.position
    legend.position <- match.arg(legend.position)
    
    # get arguments about faceting
    facet <- match.arg(facet)
    scales <- match.arg(scales)
    
    # build the string to call the helper functions
    facet <- sprintf("%s_helper", facet)
    
    # build facets, could be NULL or facet_wrap or facet_grid
    facet <- do.call(facet, args=list(formula=build.formula(lhs=lhs, rhs=rhs), nrow=wrap.nrow, ncol=wrap.ncol, scales=scales))
    
    # start plotting
    p <- ggplot(data) + 
        # lat/long and group
        aes_string(x=longitude, y=latitude, group="group") + 
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
        scale_fill_gradient2(labels=formatter, space=space, low=fill.color.low, high=fill.color.high) + 
        # title of plot
        opts(title=title, plot.title=theme_text(size=title.size, hjust=title.hjust)) +
        # faceting if called for
        facet
    
    return(p)
}


#' facet helper functions
#' 
#' Call facet functions
#' 
#' These functions merely serve to call \code{\link{facet_wrap}}, \code{\link{facet_grid}} or \code{\link{none}}.  It is needed because the program doesn't know if \code{link{facet_wrap}} or \code{\link{facet_grid}} is being called and hence doesn't know what arguments to pass, and those functions are not equiped with \dots.
#' 
#' @author Jared P. Lander
#' @aliases facet_wrap_helper facet_grid_helper none_helper
#' @seealso facet_wrap facet_grid
#' @import ggplot2
#' @param formula \code{\link{formula}} for use in \code{link{facet_wrap}}
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param scales Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y").
#' @param \dots Catch all.
#' @return The result of \code{link{facet_wrap}}.
#' 
facet_wrap_helper <- function(formula, nrow=NULL, ncol=NULL, 
                              scales=c("fixed", "free", "free_y", "free_x"), ...)
{
    scales <- match.arg(scales)
    
    facet_wrap(formula, nrow=nrow, ncol=ncol, scales=scales)
}



facet_grid_helper <- function(formula, 
                              scales=c("fixed", "free", "free_y", "free_x"), ...)
{
    scales <- match.arg(scales)
    
    facet_grid(formula, scales=scales)
}


none_helper <- function(...)
{
    none(...)
}



#' none
#' 
#' Just Null
#' 
#' Returns NULL regardless of what is put in.
#' 
#' @author Jared P. Lander
#' @aliases none
#' @export none
#' @param \dots Anything.
#' @return \code{\link{NULL}} regardless of inputs.
#' @examples
#' 
#' none()
#' none(1)
#' none(mean)
#' 
none <- function(...)
{
    return(NULL)
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