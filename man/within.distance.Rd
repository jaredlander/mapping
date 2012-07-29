\name{within.distance}
\alias{within.distance}
\title{within.distance}
\usage{
  within.distance(x, y, distance = 1, units = "miles")
}
\arguments{
  \item{x}{Two-column matrix of latitude/longitude
  coordinates.}

  \item{y}{Two-column matrix of latitude/longitude
  coordinates.}

  \item{distance}{Vector of thresholds for whether points
  in y are close to points in x.}

  \item{units}{The unit of measurement that distance is
  in.}
}
\value{
  Matrix with nrows=nrow(x) and ncols=length(distance)
}
\description{
  Compute distance threshold
}
\details{
  Compute how many points in y fall within a certain
  distance of x.
}
\author{
  Jared P. Lander
}

