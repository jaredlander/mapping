\name{prepare.shape.fortify}
\alias{prepare.shape.fortify}
\title{prepare.shape.fortify}
\usage{
  prepare.shape.fortify(shape)
}
\arguments{
  \item{shape}{A shape object as read using maptools.}
}
\value{
  A \code{\link{data.frame}} suitable for use in
  \code{ggplot}.
}
\description{
  Prepare Shape File for Plotting
}
\details{
  Takes in a shape object, fortifies it and joins in it's
  accompanying data.
}
\examples{
\dontrun{
nyc <- readShapeSpatial(fn="nyct2010.shp")
nyc.df <- prepare.shape.fortify(nyc)
}
}
\author{
  Jared P. Lander
}
\seealso{
  prepare.shape
}

