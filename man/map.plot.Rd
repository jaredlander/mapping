\name{map.plot}
\alias{map.plot}
\title{map.plot}
\usage{
  map.plot(data, variable = NULL, longitude = "long",
    latitude = "lat", fill.color.low = muted("red"),
    fill.color.mid = "white",
    fill.color.high = muted("green"), space = "Lab",
    midpoint = mean(data[[variable]], na.rm = TRUE),
    path.color = "white", title = NULL, title.size = 15,
    title.hjust = 0.5, xlab = NULL, ylab = NULL,
    barheight = 15, formatter = percent,
    legend.position = c("right", "bottom", "left", "top", "none"),
    lhs = NULL, rhs = NULL,
    facet = c("none", "facet_wrap", "facet_grid"),
    wrap.nrow = NULL, wrap.ncol = NULL,
    scales = c("fixed", "free", "free_y", "free_x"),
    gradient = "2")
}
\arguments{
  \item{data}{\code{\link{data.frame}} resulting from using
  \code{\link{fortify}} on a shapefile object.}

  \item{variable}{Character indicating which column should
  be plotted.}

  \item{longitude}{Character indicating which column stores
  longitude values.}

  \item{latitude}{Character indicating which column stores
  latitude values.}

  \item{fill.color.low}{Color to use for bottom of
  gradient.}

  \item{fill.color.mid}{Color to use for middle of
  gradient.}

  \item{fill.color.high}{Color to use for top of gradient.}

  \item{space}{Color space to use for gradient.}

  \item{midpoint}{Position of mid point of scale, defaults
  to 0.}

  \item{path.color}{Color to use for shapefile lines.}

  \item{title}{Title of plot.}

  \item{title.size}{Size of \code{Title} font.}

  \item{title.hjust}{Horizontal adjustment of \code{Title}
  position.}

  \item{xlab}{X-axis label.}

  \item{ylab}{Y-axis label.}

  \item{barheight}{Height of legend bar.}

  \item{formatter}{How the legend numbers should be
  formatted.  Takes a function.}

  \item{legend.position}{Position of legend.}

  \item{lhs}{Left side of formula to use for facetting.}

  \item{rhs}{Right side of formula to use for facetting.}

  \item{facet}{The type of facetting, if any, to use.}

  \item{wrap.nrow}{The number of rows to to use when using
  facet_wrap.}

  \item{wrap.ncol}{The number of columns to to use when
  using facet_wrap.}

  \item{scales}{The scales to be employed when faceting.}

  \item{gradient}{1 means scale_colour_gradient, 2 means
  scale_colour_gradient2}
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

