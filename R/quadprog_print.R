#' @title Print Method for Custom SVM
#' @export
print.custom_svm <- function(x, ...) {
  cat("Custom SVM Model\n")
  cat("----------------\n")
  cat("Kernel:", x$kernel, "\n")
  cat("Support Vectors:", length(x$sv_indices), "\n")
  cat("Classes:", paste(x$classes, collapse = ", "), "\n")

  if (x$kernel == "polynomial") {
    cat("Degree:", x$kernel_params$degree, "\n")
    cat("Gamma:", x$kernel_params$gamma, "\n")
    cat("Coef0:", x$kernel_params$coef0, "\n")
  } else if (x$kernel == "radial") {
    cat("Gamma:", x$kernel_params$gamma, "\n")
  }

  cat("Cost (C):", x$kernel_params$C, "\n")
  cat("Bias (b):", round(x$b, 4), "\n")

  invisible(x)
}
