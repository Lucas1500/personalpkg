#'
#' The log summed exponential function
#'
#' @param n  A positive integer value indicating the sample size
#' @param x  A vector of numeric values (set to i, sequence of positive integers
#'           from 1 to the sample size n, as default)
#'
#' @examples
#' # basic usage of log_summed_exps
#' log_summed_exps(10)
#'
#' # elaborate usage of log_summed_exps
#' log_summed_exps(5, c(1,43,6,54,123))
#'
#' @return The log summed exponential of the input vector
#'
#' @export
log_summed_exps <- function(n, x=seq(1:n)) {
  x_sort <- sort(x, decreasing = TRUE)
  x_sort[1] + log(1 + sum( exp(x_sort[2:(n-1)] - x_sort[1]) ) )
}
