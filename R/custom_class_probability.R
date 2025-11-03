#' Compute Class Probabilities from a Vector of Class Labels
#'
#' Internal helper function that converts a vector of class labels into a
#' numeric probability vector for each class. Used for estimating class
#' probabilities at leaf nodes in a decision tree or SVM-based oblique tree.
#'
#' @param vec A vector of class labels (factor or character)
#' @param all_levels A character vector of all possible class levels
#' @return A named numeric vector of probabilities summing to 1
#' @keywords internal
class_probabilities <- function(vec, all_levels) {
  f <- factor(vec, levels = all_levels)
  counts <- tabulate(f, nbins = length(all_levels))
  if (sum(counts) == 0) {
    # fallback to uniform if empty (shouldn't normally happen)
    probs <- rep(1 / length(all_levels), length(all_levels))
  } else {
    probs <- counts / sum(counts)
  }
  names(probs) <- all_levels
  probs
}
