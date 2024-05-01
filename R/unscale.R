#' Unscale a Scaled Vector
#'
#' This function reverses the centering and scaling transformations applied to a vector `x`.
#'
#' @param x A numeric vector that has been scaled.
#' @return A numeric vector with the scaling and centering reversed.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' scaled_x <- scale(x)
#' unscale(scaled_x)
#' @export


unscale <- function(x) {
  x_mean <- attr(x, "scaled:center")
  x_scale <- attr(x, "scaled:scale")
  x_unscaled <- x * x_scale + x_mean
  return(x_unscaled)
}


