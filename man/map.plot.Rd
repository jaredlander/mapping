\name{map.plot}
\alias{map.plot}
\title{map.plot}
\usage{
  map.plot(data, variable,
    fill.color.high = muted("green"), space = "Lab",
    path.color = "white", title = NULL, xlab = NULL,
    ylab = NULL, barheight = 15,
    formatter = multiple_format(multiple = "K", extra = comma),
    legend.position = c("right", "bottom", "left", "top", "none"))
}
\arguments{
  \item{data}{\code{\link{data.frame}} resulting from using
  \code{\link{fortify}} on a shapefile object.}

  \item{variable}{Character indicating which column should
  be plotted.}

  \item{fill.color.high}{Color to use for top of gradient.}

  \item{space}{Color space to use for gradient.}

  \item{path.color}{Color to use for shapefile lines.}

  \item{title}{Title of plot.}

  \item{xlab}{X-axis label.}

  \item{ylav}{Y-axis label.}

  \item{barheight}{Height of legend bar.}

  \item{formatter}{How the legend numbers should be
  formatted.  Takes a function.}

  \item{legend.position}{Position of legend.}
}
\value{
  A ggplot object.
}
\description{
  Plot Maps
}
\details{
  Takes a \code{\link{data.frame}} containing shapefile
  data and plots it along with some measure.

  Will probably need to make changes for when plotting
  continuous data.
}
\examples{
\dontrun{
nyc.income <- join(nyc.df, income2010, by="Key")
mana <- nyc.income[nyc.income$County == 61, ]
map.plot(mana, "Median.Income", formatter=multiple_format(extra=dollar, multiple="K"))
}
}
\author{
  Jared P. Lander
}

