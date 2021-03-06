\name{multiple.dollar}
\alias{multiple.dollar}
\title{multiple.dollar}
\usage{
  multiple.dollar(x, ...)
}
\arguments{
  \item{x}{Vector of numbers to be formatted.}

  \item{\dots}{Further arguments to be passed on to
  \code{link{multiple}}}
}
\value{
  Character vector of dollar formatted numbers.
}
\description{
  Order of Magnitude Formatter
}
\details{
  Simply a wrapper for multiple that prespecifies the extra
  dollar.
}
\examples{
require(scales)
vect <- c(1000, 1500, 23450, 21784, 875003780)
multiple.dollar(vect)
multiple.dollar(vect, multiple="k")
multiple.dollar(vect, multiple="h")

require(ggplot2)
data(diamonds)
ggplot(diamonds, aes(x=x, y=y, color=price*100)) + geom_point() + scale_color_gradient2(labels=multiple.dollar)
}
\author{
  Jared P. Lander
}

