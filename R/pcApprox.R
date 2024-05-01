#' Principal Component Approximation of Data
#'
#' This function returns an approximation of the data `x` based on a specified number of principal
#' components (`npc`). The approximated data is rescaled and recentered to match the original data dimensions.
#' The function uses PCA to reduce the dimensionality of the data, retaining only the specified number
#' of principal components to reconstruct an approximation of the original data.
#'
#' @param x A numeric matrix or data frame. The input data which is to be approximated.
#' @param npc The number of principal components to use for the approximation. This number cannot exceed
#' the number of columns in `x`. If it does, the function will stop with an error message.
#'
#' @return A numeric matrix approximating the original data using the specified number of principal components.
#' The returned data matrix has the same dimensions as the input matrix `x` and is scaled back to the
#' original scale and centering of `x`.
#'
#' @examples
#' data <- matrix(rnorm(100), ncol = 5)
#' approx_data <- pcApprox(data, 2)
#' print(approx_data)
#'
#' @export

pcApprox <- function(x, npc) {
  x_scaled <- scale(x)
  pca_fit <- prcomp(x_scaled, rank = npc)
  approx <- x_scaled %*% pca_fit$rotation[, 1:npc] %*% t(pca_fit$rotation[, 1:npc])
  approx_unscaled <- approx %*% diag(pca_fit$scale) + pca_fit$center
  return(approx_unscaled)
}


