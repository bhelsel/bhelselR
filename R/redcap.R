#' @title retrieve_redcap_report
#' @description Retrieves a report from the REDCap API
#' @param study A study acronym or name to select the token from a user's R environment (stored as STUDY_TOKEN)
#' @param report_id A numeric value representing the REDCap report to be downloaded to the user's R environment
#' @return A data frame containing the extracted report
#' @details Retrieves a report from the REDCap API
#' @seealso 
#'  \code{\link[RCurl]{postForm}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname retrieve_redcap_report
#' @export 
#' @importFrom RCurl postForm
#' @importFrom jsonlite fromJSON

retrieve_redcap_report <- function(study, report_id) {
  SERVER <- Sys.getenv("REDCAP_SERVER")
  TOKEN <- Sys.getenv(sprintf("%s_TOKEN", toupper(study)))
  request <- RCurl::postForm(
    uri = SERVER,
    token = TOKEN,
    content = 'report',
    format = 'json',
    report_id = report_id
  )
  jsonlite::fromJSON(request)
}
