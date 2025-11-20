#' @title Compute Kernel Matrix
#' @description
#' Computes kernel matrix K(X1, X2) for different kernel types.
#' @keywords internal
compute_kernel_matrix <- function(X1, X2, kernel, gamma, degree, coef0) {

  n1 <- nrow(X1)
  n2 <- nrow(X2)

  if (kernel == "linear") {
    # Linear kernel: K(x, y) = x^T * y
    K <- tcrossprod(X1, X2)

  } else if (kernel == "polynomial") {
    # Polynomial kernel: K(x, y) = (gamma * x^T * y + coef0)^degree
    K <- tcrossprod(X1, X2)
    K <- (gamma * K + coef0)^degree

  } else if (kernel == "radial") {
    # RBF kernel: K(x, y) = exp(-gamma * ||x - y||^2)
    # Efficient computation using: ||x-y||^2 = ||x||^2 + ||y||^2 - 2*x^T*y

    X1_sq <- rowSums(X1^2)
    X2_sq <- rowSums(X2^2)

    # Compute squared Euclidean distances
    distances_sq <- outer(X1_sq, X2_sq, "+") - 2 * tcrossprod(X1, X2)

    # Ensure non-negative (numerical stability)
    distances_sq <- pmax(distances_sq, 0)

    K <- exp(-gamma * distances_sq)

  } else {
    stop("Unsupported kernel type")
  }

  return(K)
}
