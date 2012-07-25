#' distance.matrix
#' 
#' Distance multipliers for distance conversion
#' 
#' This is a conversion for common distance conversions such as kilometers to miles or feet to yards.
#' 
#' @aliases distance.matrix
#' @author Jared P. Lander and Nicholas Galasinao
#' @return A square matrix of distance multipliers
#' @examples
#' 
#' \dontrun{
#' mapping:::distance.matrix()
#' }
#' 
distance.matrix <- function()
{
    theUnits <- c("meters", "miles", "kilometers", "feet", "yards")
    distMat <- matrix(NA, nrow=length(theUnits), ncol=length(theUnits), dimnames=list(theUnits, theUnits))
    diag(distMat) <- 1
    distMat["meters", "miles"] <- 1/1609.344
    distMat["miles", "meters"] <- 1609.344
    distMat["meters", "kilometers"] <- 1/1000
    distMat["kilometers", "meters"] <- 1000
    distMat["feet", "yards"] <- 1/3
    distMat["yards", "feet"] <- 3
    distMat["miles", "kilometers"] <- 1.609344
    distMat["kilometers", "miles"] <- 1/1.609344
    distMat["feet", "meters"] <- .3048
    distMat["meters", "feet"] <- 1/.3048
    distMat["feet", "miles"] <- 1/5280
    distMat["miles", "feet"] <- 5280
    distMat["feet", "kilometers"] <- .0003048
    distMat["kilometers", "feet"] <- 1/.0003048
    distMat["yards", "meters"] <- .9144
    distMat["meters", "yards"] <- 1/.9144
    distMat["yards", "miles"] <- 1/1760
    distMat["miles", "yards"] <- 1760
    distMat["yards", "kilometers"] <- .0009144
    distMat["kilometers", "yards"] <- 1/.0009144
    return(distMat)
}


#' unit.multiplier
#' 
#' Finds the conversion multiplier from one unit to another.
#' 
#' Finds the conversion multiplier from one unit to another.
#' 
#' @author Jared P. Lander
#' @export unit.multiplier
#' @aliases unit.multiplier
#' @param unit Type of unit to be converted.
#' @param from The units to be converted from.
#' @param to The units to be converted to.
#' @return A matrix of multipliers for converting from to to.
#' @examples
#' 
#' unit.multiplier()
#' unit.multiplier(from="meters", to="miles")
#' unit.multiplier(from=c("meters", "feet"), to="miles")
#' unit.multiplier(from="feet", to=c("miles", "kilometers"))
#' unit.multiplier(from=c("meters", "feet"), to=c("miles", "kilometers"))
#' unit.multiplier(from=c("meters", "feet"), to=c("miles", "kilometers"), unit="distance")
#' 
unit.multiplier <- function(unit="distance", from=rownames(unitMat), to=colnames(unitMat))
{
    # get matrix of multipliers
    unitMat <- do.call(sprintf("%s.matrix", unit), args=list())
    
    # make sure the selected from/to are possible
    if(any(!from %in% rownames(unitMat)) || any(!to %in% colnames(unitMat)))
    {
        stop("You selected an invalid unit")
    }

    # get the desired results
    unitMat[from, to, drop=FALSE]
}


#' dist.multiplier
#' 
#' Finds the conversion multiplier from one unit of distance to another.
#' 
#' Finds the conversion multiplier from one unit of distance to another.
#' 
#' @author Jared P. Lander
#' @export dist.multiplier
#' @aliases dist.multiplier
#' @param from The units to be converted from.
#' @param to The units to be converted to.
#' @return A matrix of multipliers for converting from to to.
#' @examples
#' 
#' dist.multiplier()
#' dist.multiplier(from="meters", to="miles")
#' dist.multiplier(from=c("meters", "feet"), to="miles")
#' dist.multiplier(from="feet", to=c("miles", "kilometers"))
#' dist.multiplier(from=c("meters", "feet"), to=c("miles", "kilometers"))
#' 
dist.multiplier <- function(from="miles", to="feet")
{
    unit.multiplier(unit="distance", from=from, to=to)
}
