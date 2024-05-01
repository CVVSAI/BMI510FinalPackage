#' Calculate Maximized Log-Likelihood for Bernoulli Trials
#'
#' This function calculates the parameter `p` that maximizes the log-likelihood
#' for a given vector of Bernoulli trials.
#'
#' @param data A numeric vector containing 0s and 1s representing Bernoulli trial outcomes.
#' @return A list containing the maximized log-likelihood and the corresponding `p` value.
#' @examples
#' data <- c(1, 0, 0, 1, 1, 0, 1)
#' logLikBernoulli(data)
#' @export


logLikBernoulli <- function(data) {
  best_p <- 0
  max_ll <- -Inf
  for (p in seq(0, 1, by = 0.001)) {
    ll <- sum(dbinom(data, size = 1, prob = p, log = TRUE))
    if (ll > max_ll) {
      best_p <- p
      max_ll <- ll
    }
  }
  return(list(p = best_p, logLik = max_ll))
}

