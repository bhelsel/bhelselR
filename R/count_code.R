#' Count Total Lines of Code in R Scripts
#'
#' Counts the total number of lines across all R script files in a specified
#' directory. This is useful for getting a quick measure of codebase size.
#'
#' @param path Character string specifying the directory path containing R script
#'   files. Default is \code{"R/"}, the standard location for R package source code.
#' @param pattern Character string specifying a regular expression pattern to
#'   filter files. Default is \code{"\\.R$"} which matches files ending in ".R".
#'   Use \code{"\\.r$|\\.R$"} to match both uppercase and lowercase extensions.
#' @param recursive Logical indicating whether to search subdirectories recursively.
#'   Default is \code{FALSE}.
#'
#' @return An integer representing the total number of lines across all matching
#'   R script files. Returns 0 if no files are found or if the directory doesn't exist.
#'
#' @details
#' This function uses \code{R.utils::countLines()} to count lines in each file,
#' which counts all lines including blank lines and comments. The function will:
#' \itemize{
#'   \item Search for files matching the specified pattern in the given directory
#'   \item Count lines in each file individually
#'   \item Sum the total across all files
#'   \item Return 0 if no matching files are found
#' }
#'
#' Note that this counts physical lines of code, not logical lines or statements.
#' Blank lines and comment lines are included in the count.
#'
#' @examples
#' \dontrun{
#' # Count lines in default R/ directory
#' count_lines()
#'
#' # Count lines in a specific directory
#' count_lines("src/")
#'
#' # Count lines recursively in all subdirectories
#' count_lines("R/", recursive = TRUE)
#'
#' # Count lines in both .R and .r files
#' count_lines(pattern = "\\.[Rr]$")
#' }
#'
#' @seealso \code{\link[R.utils]{countLines}}
#'
#' @export
count_lines <- function(path = "R/", pattern = "\\.R$", recursive = FALSE) {
  if (!dir.exists(path)) {
    warning("Directory '", path, "' does not exist. Returning 0.")
    return(0L)
  }
  
  files <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )
  
  if (length(files) == 0) {
    message("No files found matching pattern '", pattern, "' in '", path, "'")
    return(0L)
  }
  
  total_lines <- sum(purrr::map_int(files, R.utils::countLines))
  
  message(
    "Total lines: ", total_lines, 
    " across ", length(files), " file(s)"
  )
  
  return(total_lines)
}
