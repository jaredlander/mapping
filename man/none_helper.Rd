\name{none_helper}
\alias{facet_grid_helper}
\alias{facet_wrap_helper}
\alias{none_helper}
\title{none_helper}
\usage{
  none_helper(...)
}
\arguments{
  \item{\dots}{Catch all.}
}
\value{
  \code{\link{NULL}}.
}
\description{
  Call none
}
\details{
  This function merely serves to call \code{link{none}}.
  It is needed because the program doesn't know if
  \code{link{facet_wrap}} or \code{\link{facet_grid}} is
  being called and hence doesn't know what arguments to
  pass, and those functions are not equiped with \dots.
}
\author{
  Jared P. Lander
}
\seealso{
  facet_wrap facet_grid none
}

