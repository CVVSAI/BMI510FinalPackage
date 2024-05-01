#' Calculate Minimum Sample Size for One-sample or Two-sample t-tests
#'
#' This function calculates the minimum sample size required to achieve a power of 0.80
#' and a significance level of 0.05 for one-sample or two-sample t-tests, depending on
#' whether one or two vectors of data are provided.
#'
#' @param x1 Numeric vector of data for the first sample.
#' @param x2 Optional numeric vector of data for the second sample. If not provided,
#'           the function will perform a one-sample t-test analysis assuming the mean
#'           of the population is 0.
#'
#' @return The minimum sample size needed to achieve the desired statistical power and
#'         significance level for the given effect size and test type.
#'
#' @examples
#' # For one-sample t-test
#' sample1 <- rnorm(50, mean = 5, sd = 1.5)
#' minimumN(sample1)
#'
#' # For two-sample t-test
#' sample2 <- rnorm(50, mean = 5.5, sd = 1.5)
#' minimumN(sample1, sample2)
#'
#' @export
#' @importFrom pwr pwr.t.test


# Wrapper function for calculating required sample size
minimumN <- function(x1, x2 = NULL) {
  # Calculate mean and standard deviation for the first sample
  m1 <- mean(x1)
  sd1 <- sd(x1)

  if (is.null(x2)) {
    # One-sample t-test scenario
    # Hypothesis: |mu1| == 0
    # Calculate effect size
    d <- abs(m1 / sd1)
    # Calculate the required sample size
    power_analysis <- pwr::pwr.t.test(d = d, sig.level = 0.05, power = 0.80, type = "one.sample")
  } else {
    # Two-sample t-test scenario
    # Calculate mean and standard deviation for the second sample
    m2 <- mean(x2)
    sd2 <- sd(x2)
    # Calculate pooled standard deviation
    n1 <- length(x1)
    n2 <- length(x2)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    # Calculate effect size
    d <- abs(m1 - m2) / pooled_sd
    # Calculate the required sample size
    power_analysis <- pwr::pwr.t.test(d = d, sig.level = 0.05, power = 0.80, type = "two.sample", alternative = "two.sided")
  }

  return(ceiling(power_analysis$n))
}




