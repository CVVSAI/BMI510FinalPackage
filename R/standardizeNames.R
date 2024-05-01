#' Standardize Variable Names
#'
#' This function standardizes the names of variables in a data frame `data` to a specified case.
#' It first converts all variable names to snake case using `janitor::clean_names` and then
#' transforms them to the specified case using `snakecase::to_any_case`.
#'
#' @param data A data frame whose variable names are to be standardized.
#' @param case A character string specifying the desired case format for variable names.
#'             Supported cases include "small_camel", "big_camel", "snake", etc.
#' @return A data frame with standardized variable names.
#' @examples
#' data <- data.frame(X1 = 1:10, variableTwo = rnorm(10))
#' standardized_data <- standardizeNames(data)
#' @export
#' @importFrom janitor clean_names
#' @importFrom dplyr rename_with
#' @importFrom snakecase to_any_case

standardizeNames <- function(data, case = "small_camel") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  data <- janitor::clean_names(data, case = "snake")
  dplyr::rename_with(data, .fn = ~snakecase::to_any_case(.x, case = case))
}



