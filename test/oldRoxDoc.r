#' facet_wrap_helper
#' 
#' Call facet_wrap
#' 
#' This function merely serves to call \code{\link{facet_wrap}}.  It is needed because the program doesn't know if \code{\link{facet_wrap}} or \code{\link{facet_grid}} is being called and hence doesn't know what arguments to pass, and those functions are not equiped with \dots.
#' 
#' @author Jared P. Lander
#' @aliases facet_wrap_helper facet_grid_helper none_helper
#' @seealso facet_wrap facet_grid
#' @import ggplot2
#' @param formula \code{\link{formula}} for use in \code{link{facet_wrap}}
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param scales Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y").
#' @param \dots Catch all.
#' @return The result of \code{link{facet_wrap}}.
#'



#' facet_grid_helper
#' 
#' Call facet_grid
#' 
#' This function merely serves to call \code{\link{facet_grid}}.  It is needed because the program doesn't know if \code{\link{facet_wrap}} or \code{\link{facet_grid}} is being called and hence doesn't know what arguments to pass, and those functions are not equiped with \dots.
#' 
#' @author Jared P. Lander
#' @aliases facet_wrap_helper facet_grid_helper none_helper
#' @seealso facet_wrap facet_grid
#' @import ggplot2
#' @param formula \code{\link{formula}} for use in \code{link{facet_grid}}
#' @param scales Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y").
#' @param \dots Catch all.
#' @return The result of \code{link{facet_grid}}.



#' none_helper
#' 
#' Call none
#' 
#' This function merely serves to call \code{]link{none}}.  It is needed because the program doesn't know if \code{]link{facet_wrap}} or \code{\link{facet_grid}} is being called and hence doesn't know what arguments to pass, and those functions are not equiped with \dots.
#' 
#' @author Jared P. Lander
#' @aliases facet_wrap_helper facet_grid_helper none_helper
#' @seealso facet_wrap facet_grid none
#' @import ggplot2
#' @param \dots Catch all.
#' @return \code{\link{NULL}}.
#'