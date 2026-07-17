#' Retrieve PubMed references and optionally write to a BibTeX file
#'
#' Searches PubMed using one or more query terms or PMIDs and returns the
#' resulting references as a `BibEntry` object. References can optionally be
#' written to a BibTeX (`.bib`) file.
#'
#' @param ... One or more PubMed search terms or PMIDs. Multiple values are
#'   combined using `sep`.
#' @param sep Character used to combine multiple search terms. Defaults to `"|"`,
#'   which performs an OR search.
#' @param results Maximum number of records to retrieve. Defaults to `20`.
#' @param field Optional PubMed search field (e.g., `"PMID"`, `"AUTH"`,
#'   `"TITL"`). Passed to `RefManageR::ReadPubMed()`.
#' @param output Path to the BibTeX file when `write = TRUE`.
#' @param write Logical indicating whether to write the references to a BibTeX
#'   file.
#' @param append Logical indicating whether to append to an existing BibTeX file.
#'
#' @return A `BibEntry` object.
#'
#' @export

fetch_pubmed <- function(
  ...,
  sep = "|",
  results = 20,
  field = NULL,
  output = "references.bib",
  write = FALSE,
  append = TRUE
) {
  query <- unlist(list(...), use.names = FALSE)

  if (length(query) == 0) {
    stop(
      "At least one PubMed search term or PMID must be supplied.",
      call. = FALSE
    )
  }

  query_string <- paste(query, collapse = sep)

  references <- RefManageR::ReadPubMed(
    query_string,
    field = field,
    retmax = max(results, length(query))
  )

  if (write) {
    RefManageR::WriteBib(
      references,
      file = output,
      append = append
    )
  }

  references
}
