#' within.distance
#' 
#' Compute distance threshold
#' 
#' Compute how many points in y fall within a certain distance of x.
#' 
#' @author Jared P. Lander
#' @aliases within.distance
#' @export within.distance
#' @import geosphere
#' @import plyr
#' @param x Two-column matrix of latitude/longitude coordinates.
#' @param y Two-column matrix of latitude/longitude coordinates.
#' @param distance Vector of thresholds for whether points in y are close to points in x.
#' @param units The unit of measurement that distance is in.
#' @return Matrix with nrows=nrow(x) and ncols=length(distance)
within.distance <- function(x, y, distance=1, units="miles")
{
    # calculate the distance between every point in x and every point in y
    theDist <- distm(x=x, y=y, fun=distHaversine)
    
    # convert to the desired units
    theDist <- theDist * dist.multiplier(from="meters", to="miles")
    
    # check if
    result <- sapply(distance, FUN=within.distance.compute, dist.matrix=theDist)
    rownames(result) <- rownames(x)
    colnames(result) <- sprintf("%s.%s", distance, units)
    
    return(result)
}

#' within.distance.compute
#' 
#' Count number of points within distance
#' 
#' More Details coming later
#' 
#' @author Jared P. Lander
#' @aliases within.distance.compute
#' @export within.distance.compute
#' @param dist.matrix Any matrix, though typically used with distance marices.
#' @param distance The threshold to test against.
#' @return A vector with the counts of how many entries for each row are within the threshold.
within.distance.compute <- function(dist.matrix, distance=1)
{
    # check which entries fall withint the given distance
    rowSums(dist.matrix <= distance)
}