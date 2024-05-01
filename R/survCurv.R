#' Plot a Survival Curve
#'
#' This function takes a numerical vector `status` and a numerical vector `time`, calculates the survival
#' curve, and plots it using ggplot2.
#' @param status Numeric vector indicating the event status
#' @param time Numeric vector with the time to event or censoring
#' @import survival
#' @import ggplot2
#' @return A ggplot object representing the survival curve.
#' @examples
#' status <- c(1, 1, 0, 1, 0, 1)
#' time <- c(5, 10, 15, 20, 25, 30)
#' survCurv(status, time)
#' @export
#'
#'
survCurv <- function(status, time) {
  data <- data.frame(time, status)
  fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = data)
  plot(fit, xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")
}



