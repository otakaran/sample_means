#' Calculates many sample means of the same vector.
#'
#' @param vec A vector of numbers
#' @param n An integer representing the number of samples to take
#' @param reps An integer representing the number of sample means to generate
#'
#' @return A vector of numbers representing the sample means generated
#'
#' @import dplyr
#'
#' @export

many_sample_means <- function(vec, n, reps){
  # Check variable inputs
  if (!is.vector(vec)) { stop("vec is not a vector") }
  if (!is.numeric(n) & n >= 0) { stop("n is not an integer") }
  if (!is.numeric(reps) & reps >= 0) { stop("reps is not an integer") }
  
  result = NULL
  for (i in 1:reps) { result = append(result, sample_mean(vec, n)) }
  return(result)
}
