\name{multiple.identity}
\alias{multiple.identity}
\title{multiple.comma}
\usage{
  multiple.identity(x,
    multiple = c("K", "M", "B", "T", "k", "m", "b", "t"))
}
\arguments{
  \item{x}{Vector of numbers to be formatted.}

  \item{The}{multiple to display numbers in.  This symbol
  will be added to the end of the numbers.}

  \item{digits}{Number of decimal places for rounding.}
}
\value{
  Character vector of formatted numbers.
}
\description{
  Order of Magnitude Formatter
}
\details{
  Simply a wrapper for multiple that prespecifies the extra
  identity.
}
\examples{
vect <- c(1000, 1500, 23450, 21784, 875003780)
multiple.identity(vect)
multiple.identity(vect, multiple="k")
multiple.identity(vect, multiple="h")
}
\author{
  Jared P. Lander
}

