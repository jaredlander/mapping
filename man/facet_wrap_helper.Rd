\name{facet_wrap_helper}
\alias{facet_grid_helper}
\alias{facet_wrap_helper}
\alias{none_helper}
\title{facet_wrap_helper}
\usage{
  facet_wrap_helper(formula, nrow = NULL, ncol = NULL,
    scales = c("fixed", "free", "free_y", "free_x"), ...)
}
\arguments{
  \item{formula}{\code{\link{formula}} for use in
  \code{link{facet_wrap}}}

  \item{nrow}{Number of rows.}

  \item{ncol}{Number of columns.}

  \item{scales}{Should scales be fixed ("fixed", the
  default), free ("free"), or free in one dimension
  ("free_x", "free_y").}

  \item{\dots}{Catch all.}
}
\value{
  The result of \code{link{facet_wrap}}.
}
\description{
  Call facet_wrap
}
\details{
  This function merely serves to call
  \code{link{facet_wrap}}.  It is needed because the
  program doesn't know if \code{link{facet_wrap}} or
  \code{\link{facet_grid}} is being called and hence
  doesn't know what arguments to pass, and those functions
  are not equiped with \dots.
}
\author{
  Jared P. Lander
}
\seealso{
  facet_wrap facet_grid
}

