#' @title use_ku_license
#' @description Creates a roxygen2 style skeleton for standard license text for the University of Kansas Medical Center
#' @param year The copyright year (defaults to the current year), Default: format(Sys.Date(), "%Y")
#' @param copyright_holder Copyright holder (defaults to University of Kansas Medical Center within the function if no name is provided), Default: NULL
#' @param update_package Are you updating the LICENSE and LICENSE.md files in a package? Default: FALSE
#' @return Prints out a roxygen2 style skeleton in the R console for standard license text for the University of Kansas Medical Center
#' @details Creates a roxygen2 style skeleton for standard license text for the University of Kansas Medical Center
#' @seealso 
#'  \code{\link[usethis]{use_template}}
#' @rdname use_ku_license
#' @export
#' @importFrom usethis use_template proj_set
#' @importFrom rprojroot find_package_root_file
#' @importFrom desc desc_set

use_ku_license <- function(year = format(Sys.Date(), "%Y"), copyright_holder = NULL, update_package = FALSE){
  
  if(is.null(copyright_holder)) copyright_holder <- "University of Kansas Medical Center"
  
  c1 <- sprintf(paste0("#' ", "Copyright %s" ), year)
  c2 <- sprintf(paste0("#' ", "Copyright Holder: %s"), copyright_holder)
  c3 <- paste0("#' ", "All rights reserved.")
  
  data <- list(
    year = year,
    copyright_holder = copyright_holder
  )
  
  if(update_package){
    is_package <- function () {
      res <- tryCatch(rprojroot::find_package_root_file(), error = function(e) NULL)
      !is.null(res)
    }
    
    if (is_package()) {
      usethis::proj_set()
      invisible(desc::desc_set("License", "file LICENSE", file = rprojroot::find_package_root_file()))
      usethis::use_template("year-copyright.txt", save_as = "LICENSE", data = data, package = "bhelselR")
    }
    usethis::use_template("license-ku.md", save_as = "LICENSE.md", data = data, package = "bhelselR", ignore = TRUE)
  }
  
  c <- sprintf("%s\n%s\n%s", c1, c2, c3)
  
  writeLines(c)
}

