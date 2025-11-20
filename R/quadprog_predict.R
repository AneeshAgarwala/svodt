#' @title Predict Method for Custom SVM
#' @description
#' Makes predictions using a custom_svm model.
#'
#' @param object A custom_svm object from custom_svm().
#' @param newdata Matrix or data frame of new observations.
#' @param decision.values Logical; if TRUE, returns decision values.
#' @param ... Additional arguments (ignored).
#'
#' @return Vector of predicted class labels, with optional decision values as attribute.
#'
#' @export
predict.custom_svm <- function(object, newdata, decision.values = FALSE, ...) {

  if (!inherits(object, "custom_svm")) {
    stop("object must be of class 'custom_svm'")
  }

  if (!is.matrix(newdata)) {
    newdata <- as.matrix(newdata)
  }

  # Compute kernel between new data and support vectors
  K <- compute_kernel_matrix(
    newdata,
    object$sv_X,
    object$kernel,
    object$kernel_params$gamma,
    object$kernel_params$degree,
    object$kernel_params$coef0
  )

  # Compute decision values: f(x) = sum(alpha_i * y_i * K(x_i, x)) + b
  dec_vals <- as.vector(K %*% (object$alpha * object$sv_y)) + object$b

  # Classify based on sign
  predictions <- ifelse(dec_vals > 0, object$classes[1], object$classes[2])
  predictions <- factor(predictions, levels = object$classes)

  if (decision.values) {
    attr(predictions, "decision.values") <- matrix(dec_vals, ncol = 1)
  }

  return(predictions)
}
