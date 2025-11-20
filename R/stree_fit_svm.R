#' @title Fit Binary SVM for STree
#' @keywords internal
stree_fit_binary_svm <- function(X, y, kernel, verbose = FALSE, ...) {

  if (nrow(X) == 0 || length(y) == 0) {
    if (verbose) cat("Empty data, cannot fit SVM\n")
    return(list(model = NULL, left_idx = NULL, right_idx = NULL))
  }

  # Map kernel names
  svm_kernel <- switch(kernel,
                       "linear" = "linear",
                       "polynomial" = "polynomial",
                       "radial" = "radial",
                       "linear"
  )

  # Fit SVM
  model <- tryCatch(
    {
      e1071::svm(
        x = X, y = y,
        kernel = svm_kernel,
        scale = TRUE,  # STree typically scales
        decision.values = TRUE,
        ...
      )
    },
    error = function(e) {
      if (verbose) cat("SVM fitting failed:", e$message, "\n")
      NULL
    }
  )

  if (is.null(model)) {
    return(list(model = NULL, left_idx = NULL, right_idx = NULL))
  }

  # Get decision values
  dec_values <- attr(
    predict(model, X, decision.values = TRUE),
    "decision.values"
  )

  if (is.matrix(dec_values)) {
    dec_values <- dec_values[, 1]
  } else {
    dec_values <- as.numeric(dec_values)
  }

  # Split based on distance to hyperplane
  # Positive distance -> left branch, negative -> right branch
  left_idx <- which(dec_values >= 0)
  right_idx <- which(dec_values < 0)

  return(list(
    model = model,
    left_idx = left_idx,
    right_idx = right_idx
  ))
}
