\name{unit.multiplier}
\alias{unit.multiplier}
\title{unit.multiplier}
\usage{
  unit.multiplier(unit = "distance",
    from = rownames(unitMat), to = colnames(unitMat))
}
\arguments{
  \item{unit}{Type of unit to be converted.}

  \item{from}{The units to be converted from.}

  \item{to}{The units to be converted to.}
}
\value{
  A matrix of multipliers for converting from to to.
}
\description{
  Finds the conversion multiplier from one unit to another.
}
\details{
  Finds the conversion multiplier from one unit to another.
}
\examples{
unit.multiplier()
unit.multiplier(from="meters", to="miles")
unit.multiplier(from=c("meters", "feet"), to="miles")
unit.multiplier(from="feet", to=c("miles", "kilometers"))
unit.multiplier(from=c("meters", "feet"), to=c("miles", "kilometers"))
unit.multiplier(from=c("meters", "feet"), to=c("miles", "kilometers"), unit="distance")
}
\author{
  Jared P. Lander
}

