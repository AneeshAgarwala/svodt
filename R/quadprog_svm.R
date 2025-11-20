#' @title Custom SVM with Quadprog for Decision Trees
#' @description
#' Fast SVM implementation using quadprog to solve the dual problem.
#' Optimized for binary classification in decision tree nodes.
#'
#' @param X Numeric matrix or data frame of features (n x p).
#' @param y Factor or character vector of binary class labels.
#' @param kernel Kernel type: "linear", "polynomial", or "radial".
#' @param C Regularization parameter (cost). Default = 1.
#' @param degree Degree for polynomial kernel. Default = 3.
#' @param gamma Kernel coefficient. If NULL, defaults to 1/ncol(X).
#' @param coef0 Independent term in polynomial kernel. Default = 0.
#' @param tol Tolerance for support vector selection. Default = 1e-5.
#' @param verbose Logical; print optimization details.
#'
#' @return A list with class "custom_svm" containing:
#'   \item{alpha}{Lagrange multipliers for support vectors}
#'   \item{sv_indices}{Indices of support vectors}
#'   \item{sv_X}{Support vector features}
#'   \item{sv_y}{Support vector labels (as +1/-1)}
#'   \item{b}{Bias term}
#'   \item{kernel}{Kernel type used}
#'   \item{kernel_params}{List of kernel parameters}
#'   \item{classes}{Original class labels}
#'
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(100 * 2), ncol = 2)
#' y <- factor(sample(c("A", "B"), 100, replace = TRUE))
#' model <- custom_svm(X, y, kernel = "linear", C = 1)
#' predictions <- predict(model, X)
#' }
#'
#' @export
custom_svm <- function(X, y, kernel = c("linear", "polynomial", "radial"),
                       C = 1, degree = 3, gamma = NULL, coef0 = 0,
                       tol = 1e-5, verbose = FALSE) {

  # Input validation
  kernel <- match.arg(kernel)

  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }

  # Ensure numeric matrix
  if (!is.numeric(X)) {
    stop("X must be numeric")
  }

  n <- nrow(X)
  p <- ncol(X)

  # Store original classes and convert to +1/-1
  classes <- levels(factor(y))
  if (length(classes) != 2) {
    stop("custom_svm only supports binary classification")
  }

  y_numeric <- ifelse(y == classes[1], 1, -1)

  # Set default gamma
  if (is.null(gamma)) {
    gamma <- 1 / p
  }

  # Store kernel parameters
  kernel_params <- list(
    kernel = kernel,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    C = C
  )

  if (verbose) {
    cat("Training custom SVM with", kernel, "kernel\n")
    cat("Samples:", n, "Features:", p, "\n")
  }

  # Compute kernel matrix
  K <- compute_kernel_matrix(X, X, kernel, gamma, degree, coef0)

  # Set up quadratic programming problem
  # Dual problem: maximize W(alpha) = sum(alpha) - 0.5 * sum(alpha_i * alpha_j * y_i * y_j * K(x_i, x_j))
  # Subject to: 0 <= alpha_i <= C and sum(alpha_i * y_i) = 0

  # Quadprog solves: min(-d^T*alpha + 1/2*alpha^T*D*alpha)
  # So we need to negate the objective

  # D matrix: y_i * y_j * K(x_i, x_j)
  D <- outer(y_numeric, y_numeric) * K

  # Make D symmetric (numerical stability)
  D <- (D + t(D)) / 2

  # Add small ridge for numerical stability
  D <- D + diag(1e-8, n)

  # d vector: all ones (we want to maximize sum of alphas)
  d <- rep(1, n)

  # Constraints: A^T * alpha >= b
  # 1. y^T * alpha = 0 (equality constraint, convert to two inequalities)
  # 2. alpha >= 0
  # 3. alpha <= C

  # Build constraint matrix
  A <- rbind(
    y_numeric,           # sum(alpha_i * y_i) = 0
    -y_numeric,          # sum(alpha_i * y_i) = 0 (other direction)
    diag(n),             # alpha_i >= 0
    -diag(n)             # alpha_i <= C (i.e., -alpha_i >= -C)
  )

  b <- c(0, 0, rep(0, n), rep(-C, n))

  # Solve using quadprog
  solution <- tryCatch({
    quadprog::solve.QP(
      Dmat = D,
      dvec = d,
      Amat = t(A),
      bvec = b,
      meq = 2  # First two constraints are equality
    )
  }, error = function(e) {
    if (verbose) cat("Quadprog failed:", e$message, "\n")
    return(NULL)
  })

  if (is.null(solution)) {
    return(NULL)
  }

  alpha <- solution$solution

  # Identify support vectors (alpha > tol)
  sv_indices <- which(alpha > tol)

  if (length(sv_indices) == 0) {
    if (verbose) cat("No support vectors found\n")
    return(NULL)
  }

  # Extract support vectors
  sv_alpha <- alpha[sv_indices]
  sv_X <- X[sv_indices, , drop = FALSE]
  sv_y <- y_numeric[sv_indices]

  if (verbose) {
    cat("Support vectors:", length(sv_indices), "/", n, "\n")
  }

  # Calculate bias term b
  # For support vectors with 0 < alpha < C (on margin)
  margin_sv <- which(alpha > tol & alpha < (C - tol))

  if (length(margin_sv) > 0) {
    # Use support vectors on the margin to compute b
    b_values <- sapply(margin_sv, function(i) {
      K_i <- compute_kernel_matrix(X[i, , drop = FALSE], sv_X,
                                   kernel, gamma, degree, coef0)
      y_numeric[i] - sum(sv_alpha * sv_y * K_i)
    })
    b <- mean(b_values)
  } else {
    # No margin support vectors, use all support vectors
    if (verbose) cat("No margin SVs, using all SVs for bias\n")
    K_sv <- compute_kernel_matrix(sv_X[1, , drop = FALSE], sv_X,
                                  kernel, gamma, degree, coef0)
    b <- sv_y[1] - sum(sv_alpha * sv_y * K_sv)
  }

  # Return model
  structure(
    list(
      alpha = sv_alpha,
      sv_indices = sv_indices,
      sv_X = sv_X,
      sv_y = sv_y,
      b = b,
      kernel = kernel,
      kernel_params = kernel_params,
      classes = classes
    ),
    class = "custom_svm"
  )
}
