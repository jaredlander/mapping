\name{dist.multiplier}
\alias{dist.multiplier}
\title{dist.multiplier}
\usage{
  dist.multiplier(from = "miles", to = "feet")
}
\arguments{
  \item{from}{The units to be converted from.}

  \item{to}{The units to be converted to.}
}
\value{
  A matrix of multipliers for converting from to to.
}
\description{
  Finds the conversion multiplier from one unit of distance
  to another.
}
\details{
  Finds the conversion multiplier from one unit of distance
  to another.
}
\examples{
dist.multiplier()
dist.multiplier(from="meters", to="miles")
dist.multiplier(from=c("meters", "feet"), to="miles")
dist.multiplier(from="feet", to=c("miles", "kilometers"))
dist.multiplier(from=c("meters", "feet"), to=c("miles", "kilometers"))
}
\author{
  Jared P. Lander
}

