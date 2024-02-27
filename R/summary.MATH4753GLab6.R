#' summary.MATH4753GLab6
#'
#' Summarizes the binomial simulation by returning a vector of sample standard deviations
#'
#' @param object a numerical object
#' @param ... pass additional options to the argument
#'
#' @return a vector of sample sd's of length "iter"
#' @export summary.MATH4753GLab6
#'
#' @export
#'
#' @examples
#' \dontrun{summary.MATH4753GLab6)(x = mpg)}
summary.MATH4753GLab6 <- function(object, ...){
  x <- object

  x <- data.frame(sd = x$sim)

  apply(x, 2, stats::sd)
}
