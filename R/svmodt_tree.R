#' Build an Oblique Decision Tree Using SVM Splits
#'
#' Constructs a decision tree where each internal node uses a Support Vector
#' Machine (SVM) to determine the split. Supports dynamic feature selection,
#' feature penalization, scaling, and class weighting.
#'
#' @param data A data frame containing predictors and the response variable.
#' @param response Character string specifying the response column in `data`.
#'   All other columns are treated as predictors.
#' @param depth Integer indicating the current recursion depth (used internally; default is 1).
#' @param max_depth Maximum depth of the tree.
#' @param min_samples Minimum number of samples required to attempt a split.
#' @param max_features Maximum number of features to consider at each split.
#' @param feature_method Feature selection method at each node. One of:
#'   \itemize{
#'     \item `"random"`: randomly select features,
#'     \item `"mutual"`: select based on mutual information with the response,
#'     \item `"cor"`: select based on correlation with the response.
#'   }
#' @param max_features_strategy Strategy to adjust the number of features per node:
#'   \itemize{
#'     \item `"constant"`: keep `max_features` constant,
#'     \item `"decrease"`: reduce features with depth,
#'     \item `"random"`: randomly vary number of features within a range.
#'   }
#' @param max_features_decrease_rate Numeric fraction for decreasing features if
#'   `max_features_strategy = "decrease"`.
#' @param max_features_random_range Numeric vector of length 2 specifying min and max
#'   fraction of features if `max_features_strategy = "random"`.
#' @param penalize_used_features Logical; if TRUE, features used in ancestor nodes
#'   are penalized to encourage diversity.
#' @param feature_penalty_weight Numeric (0–1) weight for penalizing previously used features.
#' @param used_features Character vector of features already used in ancestor nodes
#'   (used internally).
#' @param class_weights Character string specifying how to handle class imbalance. One of:
#'   \itemize{
#'     \item `"none"`: no weighting,
#'     \item `"balanced"`: weight classes inversely proportional to their frequency,
#'     \item `"balanced_subsample"`: weight per node based on local class distribution,
#'     \item `"custom"`: use `custom_class_weights`.
#'   }
#' @param custom_class_weights Optional named numeric vector specifying custom weights per class.
#' @param all_classes Optional character vector of all possible response classes (used internally).
#' @param verbose Logical; if TRUE, prints information about each node during tree construction.
#' @param ... Additional arguments passed to the underlying SVM fitting function.
#'
#' @return A nested list representing the decision tree. Each node contains:
#' \describe{
#'   \item{is_leaf}{Logical; TRUE if the node is a leaf.}
#'   \item{model}{Fitted SVM model at this node (for internal nodes).}
#'   \item{features}{Vector of features selected for this node.}
#'   \item{scaler}{Scaling information used at this node.}
#'   \item{left}{Left child node (decision value > 0).}
#'   \item{right}{Right child node (decision value ≤ 0).}
#'   \item{depth}{Depth of this node in the tree.}
#'   \item{n}{Number of samples at this node.}
#'   \item{max_features_used}{Number of features considered at this node.}
#'   \item{penalty_applied}{Logical; TRUE if feature penalization was applied.}
#'   \item{class_weights_used}{Class weights applied at this node.}
#' }
#'
#' @details
#' This function recursively splits the dataset using an SVM at each node. Splitting
#' stops when maximum depth is reached, the node contains fewer than `min_samples`,
#' or all samples belong to the same class. Features are scaled and selected dynamically
#' at each node, and previously used features can be penalized to promote diversity.
#' Class weighting schemes support handling imbalanced datasets. This approach allows
#' construction of an **oblique decision tree**, where splits are linear hyperplanes
#' rather than axis-aligned.
#'
#' @examples
#' data(wdbc)
#' tree <- svm_split(
#'   data = wdbc,
#'   response = "diagnosis",
#'   max_depth = 3,
#'   min_samples = 5,
#'   feature_method = "random",
#'   verbose = TRUE
#' )
#'
#' @export
svm_split <- function(data, response, depth = 1, max_depth = 3,
                      min_samples = 5, max_features = NULL,
                      feature_method = c("random", "mutual", "cor"),
                      max_features_strategy = c("constant", "random", "decrease"),
                      max_features_decrease_rate = 0.8,
                      max_features_random_range = c(0.3, 1.0),
                      penalize_used_features = FALSE,
                      feature_penalty_weight = 0.5,
                      used_features = character(0),
                      class_weights = c("none", "balanced", "balanced_subsample", "custom"),
                      custom_class_weights = NULL,
                      verbose = FALSE, all_classes = NULL, ...) {
  # Initialize all_classes if NULL
  if (is.null(all_classes)) {
    all_classes <- levels(factor(data[[response]]))
  }

  # Validate inputs early
  if (!response %in% names(data)) {
    stop("Response variable '", response, "' not found in data")
  }

  if (verbose) {
    cat("\n--- Node at depth", depth, "---\n")
    cat("Rows:", nrow(data), "\n")
    cat("Response distribution:\n")
    print(table(data[[response]]))
    if (length(used_features) > 0) {
      cat("Used features in ancestors:", paste(used_features, collapse = ", "), "\n")
    }
  }

  # Handle NA rows
  if (anyNA(data)) {
    if (verbose) cat("Warning: NA values detected! Stopping here.\n")
    return(leaf_node(data[[response]], nrow(data), all_classes))
  }

  y <- data[[response]]

  # Stopping rules
  if (depth > max_depth || length(unique(y)) == 1 || nrow(data) < min_samples) {
    if (verbose) cat("Stopping at depth", depth, "\n")
    return(leaf_node(y, nrow(data), all_classes))
  }

  # Calculate class weights for this node
  node_class_weights <- calculate_node_class_weights(
    y, class_weights, custom_class_weights, verbose
  )

  # Dynamic max_features calculation
  current_max_features <- calculate_dynamic_max_features(
    data, response, max_features, depth,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    verbose
  )

  # Feature selection with penalties
  features <- if (is.null(current_max_features)) {
    setdiff(names(data), response)
  } else {
    choose_features_with_penalty(
      data, response, current_max_features, feature_method,
      penalize_used_features, feature_penalty_weight, used_features,
      verbose
    )
  }

  if (length(features) == 0) {
    if (verbose) cat("Stopping: no usable features\n")
    return(leaf_node(y, nrow(data), all_classes))
  }

  if (verbose) {
    cat("Dynamic max_features:", current_max_features, "\n")
    cat("Selected features:", paste(features, collapse = ", "), "\n")
  }

  # Scaling
  scaler <- scale_node(data[features])
  X_scaled <- scaler$train

  # Handle case where scaling fails
  if (ncol(X_scaled) == 0) {
    if (verbose) cat("Stopping: all features are constant\n")
    return(leaf_node(y, nrow(data), all_classes, features, scaler))
  }

  # Fit SVM with calculated class weights
  model <- fit_svm_with_weights(X_scaled, y, node_class_weights, verbose, ...)
  if (is.null(model)) {
    return(leaf_node(y, nrow(data), all_classes, features, scaler))
  }

  # Decision boundary
  dec <- attr(predict(model, X_scaled, decision.values = TRUE), "decision.values")
  decision_values <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)
  left_idx <- which(decision_values > 0)
  right_idx <- which(decision_values <= 0)

  if (verbose) cat("Left size:", length(left_idx), "Right size:", length(right_idx), "\n")

  # No effective split
  if (length(left_idx) == 0 || length(right_idx) == 0) {
    if (verbose) cat("Stopping: ineffective split (all points on one side)\n")
    return(leaf_node(y, nrow(data), all_classes, features, scaler))
  }

  # Pass ALL required parameters including class weights and all_classes
  child_check <- handle_small_children(
    left_idx, right_idx, min_samples,
    data, response, depth, max_depth,
    max_features, feature_method,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    penalize_used_features, feature_penalty_weight, used_features,
    class_weights, custom_class_weights,
    features, scaler, all_classes, verbose, ...
  )

  if (child_check$stop) {
    return(child_check$node)
  }
  if (!is.null(child_check$node)) {
    child_check$node$model <- model
    return(child_check$node)
  }

  # Update used features for children
  updated_used_features <- if (penalize_used_features) {
    unique(c(used_features, features))
  } else {
    used_features
  }

  # Recurse with all parameters
  left <- svm_split(
    data[left_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples,
    max_features, feature_method,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    penalize_used_features, feature_penalty_weight, updated_used_features,
    class_weights, custom_class_weights,
    verbose = verbose, all_classes = all_classes, ...
  )

  right <- svm_split(
    data[right_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples,
    max_features, feature_method,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    penalize_used_features, feature_penalty_weight, updated_used_features,
    class_weights, custom_class_weights,
    verbose = verbose, all_classes = all_classes, ...
  )

  list(
    is_leaf = FALSE,
    model = model,
    features = features,
    scaler = scaler,
    best_col = 1,
    left = left,
    right = right,
    depth = depth,
    n = nrow(data),
    max_features_used = current_max_features,
    penalty_applied = penalize_used_features && length(used_features) > 0,
    class_weights_used = node_class_weights
  )
}
