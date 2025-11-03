#' @title Build a Decision Tree with Cost-Complexity Pruning
#' @description
#' Constructs a decision tree for classification using a chosen splitting criterion
#' (Gini impurity, information gain, or gain ratio). Supports numeric and factor
#' features and integrates cost–complexity pruning via an \code{alpha} parameter.
#'
#' @param target A vector of class labels (factor or character) for the samples.
#' @param features A data frame or matrix of predictor variables.
#' @param criteria_type Splitting criterion: \code{"gini"}, \code{"info_gain"}, or \code{"gain_ratio"}.
#' @param ig_metric Impurity metric to use for \code{info_gain}: \code{"gini"} or \code{"entropy"}.
#' @param depth Internal use: current recursion depth (default = 1).
#' @param max_depth Maximum allowed depth of the tree (default = 10).
#' @param all_levels Optional vector of all possible class levels. If \code{NULL}, inferred from \code{target}.
#' @param alpha Cost-complexity pruning parameter. Higher values increase pruning (default = 0).
#'
#' @return A nested list representing the decision tree. Each node contains:
#' \itemize{
#'   \item \code{split_feature}: Name of the feature used for splitting (internal nodes only).
#'   \item \code{split_value}: Numeric threshold or factor level used for splitting (internal nodes only).
#'   \item \code{left}: Left child subtree (samples satisfying the split condition).
#'   \item \code{right}: Right child subtree.
#'   \item \code{probs}: Named vector of class probabilities at the node.
#'   \item \code{prediction}: Majority class at the node.
#'   \item \code{n}: Number of samples at the node.
#' }
#'
#' @details
#' The function recursively grows a classification tree:
#' \itemize{
#'   \item Stops if maximum depth is reached, or if the node is pure (all samples belong to one class).
#'   \item Selects the best feature and split point using the chosen \code{criteria_type}.
#'   \item Handles numeric and factor features automatically.
#'   \item Implements cost–complexity pruning: compares subtree error with leaf error, scaled by \code{alpha}.
#'   \item Returns a leaf node if no valid split exists or if pruning criterion favors collapsing the node.
#' }
#'
#' @examples
#' # Example with wdbc dataset
#' data(wdbc)
#' features <- wdbc[, 2:31]
#' target <- wdbc$diagnosis
#' tree <- generate_tree(target, features, criteria_type = "gini", max_depth = 3, alpha = 0.01)
#'
#' # Access root node prediction
#' tree$prediction
#'
#' # Access left child subtree
#' tree$left
#'
#' @export
generate_tree <- function(target, features,
                          criteria_type = c("gini", "info_gain", "gain_ratio"),
                          ig_metric = c("gini", "entropy"),
                          depth = 1, max_depth = 10,
                          all_levels = NULL, alpha = 0.0) {
  criteria_type <- match.arg(criteria_type)
  ig_metric <- match.arg(ig_metric)

  if (is.null(all_levels)) {
    all_levels <- levels(factor(target))
  }

  # node stats used for both leaf and internal nodes
  probs <- class_probabilities(target, all_levels)
  prediction <- names(probs)[which.max(probs)]
  n <- length(target)

  # stopping criteria: max depth or pure node
  if (depth >= max_depth || length(unique(target)) <= 1) {
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  }

  # choose best split
  fs <- feature_selector(target, features, criteria_type, ig_metric)
  split_feature <- fs$feature
  split_value <- fs$split

  # if no valid split, return leaf
  if (is.null(split_feature) || is.null(split_value) || is.infinite(fs$score)) {
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  }

  fvec <- features[[split_feature]]
  if (is.numeric(fvec)) {
    left_idx <- fvec <= split_value
    right_idx <- !left_idx
  } else {
    left_idx <- fvec == split_value
    right_idx <- fvec != split_value
  }

  # guard: if split creates empty child, revert to leaf
  if (sum(left_idx) == 0 || sum(right_idx) == 0) {
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  }

  # grow children
  left_subtree <- generate_tree(
    target = target[left_idx],
    features = features[left_idx, , drop = FALSE],
    criteria_type = criteria_type,
    ig_metric = ig_metric,
    depth = depth + 1,
    max_depth = max_depth,
    all_levels = all_levels,
    alpha = alpha
  )

  right_subtree <- generate_tree(
    target = target[right_idx],
    features = features[right_idx, , drop = FALSE],
    criteria_type = criteria_type,
    ig_metric = ig_metric,
    depth = depth + 1,
    max_depth = max_depth,
    all_levels = all_levels,
    alpha = alpha
  )

  # form internal node
  node <- list(
    split_feature = split_feature,
    split_value = split_value,
    left = left_subtree,
    right = right_subtree,
    probs = probs,
    prediction = prediction,
    n = n
  )

  # --- Integrated cost–complexity pruning decision ---
  # Cost of subtree
  cost_subtree <- subtree_error(node) + alpha * count_leaves(node)
  # Cost if we collapse to a leaf here
  cost_leaf <- (1 - max(probs)) + alpha * 1

  if (cost_leaf <= cost_subtree) {
    # prune: return leaf
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  } else {
    return(node)
  }
}
