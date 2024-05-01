#' Download a CSV Report from REDCap
#'
#' This function downloads a specific report from a REDCap project in CSV format
#' using an API token and report ID. The API token should be stored in an environment
#' variable whose name is passed to the function. The function returns the report
#' as a dataframe.
#'
#' @param redcapTokenName The name of the environment variable where the REDCap API token is stored.
#' @param redcapUrl The URL of the REDCap API endpoint.
#' @param redcapReportId The ID of the report to be downloaded.
#'
#' @return A dataframe containing the data from the specified REDCap report.
#'
#' @examples
#' # Example usage:
#' # downloadRedcapReport("MY_REDCAP_TOKEN", "https://redcap.example.com/api/", "12345")
#'
#' @export
#' @importFrom httr POST content
#' @importFrom utils read.csv

downloadRedcapReport <- function(redcapTokenName, redcapUrl, redcapReportId) {
  token <- Sys.getenv(redcapTokenName)
  formData <- list(
    "token" = token,
    content = 'report',
    format = 'csv',
    report_id = redcapReportId,
    csvDelimiter = '',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )
  response <- httr::POST(redcapUrl, body = formData, encode = "form")
  result <- httr::content(response, as = "text", encoding = "UTF-8")
  final_result <- read.csv(text = result, stringsAsFactors = FALSE)
  return(final_result)
}
