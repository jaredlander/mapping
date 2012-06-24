## formatting functions

#' multiple
#' 
#' Order of Magnitude Formatter
#' 
#' This divides the number by the apropriate amount and adds on the corresponding symbol at the end of the number.
#' 
#' @author Jared P. Lander
#' @aliases multiple
#' @export multiple
#' @import scales
#' @param x Vector of numbers to be formatted.
#' @param The multiple to display numbers in.  This symbol will be added to the end of the numbers.
#' @param extra Function for perform any further formatting.
#' @param digits Number of decimal places for rounding.
#' @return Character vector of formatted numbers.
#' @examples
#' vect <- c(1000, 1500, 23450, 21784, 875003780)
#' multiple(vect)
#' multiple(vect, extra=dollar)
#' multiple(vect, extra=identity)
#' 
multiple <- function(x, multiple=c("K", "M", "B", "T", "H", "k", "m", "b", "t", "h"), extra=comma, digits=0)
{
    # get the multiple
    multiple=match.arg(multiple)
    
    # set up a vector for dividing
    dividers <- c("K"=1000, "M"=1000000, "B"=1000000000, "T"=1000000000000, "H"=100)
    
    # get what we're dividing by
    divider <- dividers[toupper(multiple)]
    
    x <- round(x / divider, digits=digits)
    
    x <- do.call(extra, args=list(x))
    sprintf("%s%s", x, multiple)
}


#' multiple_format
#' 
#' 
multiple_format <- function(...)
{
    function(x) multiple(x, ...)
}


multiple.dollar <- function(x, multiple=c("K", "M", "B", "T", "k", "m", "b", "t"))
{
    multiple(x=x, multiple=multiple, extra=dollar)
}

multiple.comma <- function(x, multiple=c("K", "M", "B", "T", "k", "m", "b", "t"))
{
    multiple(x=x, multiple=multiple, extra=comma)
}