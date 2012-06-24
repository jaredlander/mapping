\name{prepare.shape}
\alias{prepare.shape}
\title{prepare.shape}
\usage{
  prepare.shape(file)
}
\arguments{
  \item{file}{Name of shape file.}
}
\value{
  A \code{\link{data.frame}} suitable for use in
  \code{ggplot}.
}
\description{
  Prepare Shape File for Plotting
}
\details{
  Reads in a shape file and then fortifies it.
}
\examples{
\dontrun{
nyc.df <- prepare.shape("nyct2010.shp")
}
}
\author{
  Jared P. Lander
}
\seealso{
  prepare.shape.fortify
}

