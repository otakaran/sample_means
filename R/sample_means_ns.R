#' Generates many sample means for each number in ns
#'
#' @param vec A vector of numbers
#' @param reps An integer representing the number of sample means to generate for all n in ns
#' @param ns A vector of n integers which will be looped trough and used to generate sample means with those n samples
#'
#' @return A tibble showing the given n used to generate each sample_mean (cols: n, column_means)
#'
#' @import dplyr
#'
#' @export

sample_means_ns <- function(vec, reps, ns) {
  # Check variable inputs
  if (!is.vector(vec)) { stop("vec is not a vector") }
  if (!is.numeric(reps) & reps >= 0) { stop("reps is not an integer") }
  if (!is.vector(ns)) { stop("ns is not an vector") }
  
  result = tibble(sample_mean = numeric(), n = numeric())
  for (n in ns) {
    result <- result %>% add_row(sample_mean = many_sample_means(vec, n, reps), n = n)
  }
  return(result)
}
