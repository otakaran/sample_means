#' Calculates the sample mean of a vector.
#'
#' @param vec A vector of numbers
#' @param n An integer representing the number of samples to take
#'
#' @return The sample mean of a vector with n items selected (with replacement)
#'
#' @import dplyr
#'
#' @export

sample_mean <- function(vec, n){
  # Check variable inputs
  if (!is.vector(vec)) { stop("vec is not a vector") }
  if (!is.numeric(n) & n >= 0) { stop("n is not an integer") }

  return(mean(sample(vec, n, replace = TRUE)))
}
