#' prepare.shape.fortify
#' 
#' Prepare Shape File for Plotting
#' 
#' Takes in a shape object, fortifies it and joins in it's accompanying data.
#' 
#' @param shape A shape object as read using maptools.
#' @return A \code{\link{data.frame}} suitable for use in \code{ggplot}.
#' @author Jared P. Lander
#' @aliases prepare.shape.fortify
#' @import ggplot2 plyr stringr maptools
#' @seealso prepare.shape
#' @examples \dontrun{
#' nyc <- readShapeSpatial(fn="nyct2010.shp")
#' nyc.df <- prepare.shape.fortify(nyc)
#' }
#' 
prepare.shape.fortify <- function(shape)
{
    # needed for fortify to work
    suppressMessages(gpclibPermit())
    # put rownames as id in the data
    shape@data$id <- rownames(shape@data)
    # fortify the shape file
    shape.points <- fortify(shape, region="id")
    # join in info from data
    shape.df <- join(shape.points, shape@data, by="id")
    #housekeeping
    rm(shape, shape.points)
    
    return(shape.df)
}


#' prepare.shape
#' 
#' Prepare Shape File for Plotting
#' 
#' Reads in a shape file and then fortifies it.
#' @author Jared P. Lander
#' @seealso prepare.shape.fortify
#' @export prepare.shape
#' @aliases prepare.shape
#' @param file Name of shape file.
#' @return A \code{\link{data.frame}} suitable for use in \code{ggplot}.
#' @examples \dontrun{
#' nyc.df <- prepare.shape("nyct2010.shp")
#' }
#' 
prepare.shape <- function(file)
{
    # read shape file
    shape <- readShapeSpatial(fn=file)
    # frotify and join the file
    prepare.shape.fortify(shape)
}