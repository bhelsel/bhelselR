#' @title find_table
#' @description Reads in an HTML page from a faculty curriculum vitae and extracts the table following the HTML tag and label
#' @param path The path name for the HTML page containing a curriculum vita downloaded from the faculty member's profile
#' @param tag The HTML tag containing label
#' @param label Text that labels the start of the HTML table
#' @return A tibble containing the table from the HTML page
#' @details Reads in an HTML page from a faculty curriculum vitae and extracts the table following the HTML tag and label
#' @seealso
#'  \code{\link[rvest]{read_html}}, \code{\link[rvest]{html_element}}, \code{\link[rvest]{html_table}}
#' @rdname find_table
#' @export
#' @importFrom rvest read_html html_element html_table

find_table <- function(path, tag, label) {
  path %>%
    rvest::read_html() %>%
    rvest::html_element(
      xpath = sprintf("//%s[contains(., '%s')]", tag, label)
    ) %>%
    rvest::html_element(xpath = "following-sibling::table[1]") %>%
    rvest::html_table(fill = TRUE)
}

#' @title extract_reference_table
#' @description Extracts the reference table from a faculty curriculum vitae
#'     containing journal articles that have been published, in press, or submitted
#' @param path The path name for the HTML page containing a curriculum vita downloaded from the faculty member's profile
#' @param years The year (e.g., 2024) or years (e.g., 2022:2024 or c(2022, 2023, 2024))
#'     of which to extract the references. If the years argument is left blank, the
#'     function retrieves references for the current year, Default: NULL
#' @param return_data_frame Combine all the journal articles that have been published,
#'     in press, or submitted into a single data frame, Default: FALSE
#' @return A list or data frame containing the references for the designated years
#' @details Extracts the reference table from a faculty curriculum vitae
#'     containing journal articles that have been published, in press, or submitted
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_remove}}
#'  \code{\link[purrr]{map2}}
#' @rdname extract_reference_table
#' @export
#' @importFrom dplyr mutate filter select bind_rows
#' @importFrom stringr str_extract str_remove
#' @importFrom purrr map2

extract_reference_table <- function(
  path,
  years = NULL,
  return_data_frame = FALSE
) {
  if (is.null(years)) years <- as.numeric(format(Sys.Date(), "%Y"))

  labels <- c(
    "Articles (Peer-Review Published)",
    "Manuscripts in Press",
    "Manuscripts Submitted"
  )

  reference_list <-
    lapply(1:length(labels), FUN = function(x) {
      path %>%
        find_table(tag = "h3", label = labels[x]) %>%
        dplyr::mutate(
          extracted_years = gsub(
            "[()]",
            "",
            stringr::str_extract(X1, "\\(\\d{4}\\)")
          ),
          References = stringr::str_remove(X1, "^\\d+\\.\\s*")
        ) %>%
        dplyr::filter(extracted_years %in% years) %>%
        dplyr::select(References)
    })

  names(reference_list) <- labels

  if (return_data_frame) {
    reference_list <-
      reference_list %>%
      purrr::map2(., labels, ~ mutate(.x, source = .y, .before = 1)) %>%
      dplyr::bind_rows()
  }

  return(reference_list)
}


#' @title extract_poster_table
#' @description Extracts the presentation table from a faculty curriculum vitae
#'     containing oral, poster, and invited presentations
#' @param path The path name for the HTML page containing a curriculum vita downloaded from the faculty member's profile
#' @param years The year (e.g., 2024) or years (e.g., 2022:2024 or c(2022, 2023, 2024))
#'     of which to extract the presentations. If the years argument is left blank, the
#'     function retrieves presentations for the current year, Default: NULL
#' @param return_data_frame Combine all the oral, poster, and invited presentations
#'     into a single data frame, Default: FALSE
#' @return A list or data frame containing the presentations for the designated years
#' @details Extracts the presentation table from a faculty curriculum vitae
#'     containing oral, poster, and invited presentations
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_remove}}
#'  \code{\link[purrr]{map2}}
#' @rdname extract_poster_table
#' @export
#' @importFrom dplyr mutate filter select bind_rows
#' @importFrom stringr str_extract str_remove
#' @importFrom purrr map2

extract_poster_table <- function(
  path,
  years = NULL,
  return_data_frame = FALSE
) {
  if (is.null(years)) years <- as.numeric(format(Sys.Date(), "%Y"))

  labels <- c(
    "Oral Paper Presentation",
    "Poster Presentation",
    "Invited Seminars at Other Universities"
  )

  reference_list <-
    lapply(1:length(labels), FUN = function(x) {
      path %>%
        find_table(tag = "h3", label = labels[x]) %>%
        dplyr::mutate(
          extracted_dates = gsub(
            "[()]",
            "",
            stringr::str_extract(X1, "\\(.*?(\\d{4})\\)")
          ),
          extracted_years = stringr::str_extract(extracted_dates, "\\d{4}"),
          Presentations = stringr::str_remove(X1, "^\\d+\\.\\s*")
        ) %>%
        dplyr::filter(extracted_years %in% years) %>%
        dplyr::select(Presentations)
    })

  names(reference_list) <- labels

  if (return_data_frame) {
    reference_list <-
      reference_list %>%
      purrr::map2(., labels, ~ mutate(.x, source = .y, .before = 1)) %>%
      dplyr::bind_rows()
  }

  return(reference_list)
}
