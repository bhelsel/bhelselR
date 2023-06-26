#' @title use_ku_license
#' @description Creates a roxygen2 style skeleton for standard license text for the University of Kansas
#' @param year The copyright year (defaults to the current year), Default: format(Sys.Date(), "%Y")
#' @param copyright_holder Copyright holder (defaults to University of Kansas within the function if no name is provided), Default: NULL
#' @param full_version Add descriptive text within the roxygen2 style skeleton printed in the R console
#' @param update_package Are you updating the LICENSE and LICENSE.md files in a package? Default: FALSE
#' @return Prints out a roxygen2 style skeleton in the R console for standard license text for the University of Kansas
#' @details Creates a roxygen2 style skeleton for standard license text for the University of Kansas
#' @seealso 
#'  \code{\link[usethis]{use_template}}
#' @rdname use_ku_license
#' @export
#' @importFrom usethis use_template proj_set
#' @importFrom rprojroot find_package_root_file
#' @importFrom desc desc_set

use_ku_license <- function(year = format(Sys.Date(), "%Y"), copyright_holder = NULL, full_version = FALSE, update_package = FALSE){
  
  if(is.null(copyright_holder)) copyright_holder <- "University of Kansas"
  
  c <- sprintf(paste0("#' ", "Copyright \U00A9 %s %s. All rights reserved." ), year, copyright_holder)
  
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
  
  if(full_version){
    cat(
      c, 
      "\n#' Permission is hereby granted, free of charge, to any person obtaining a copy",
      "\n#' of this software and associated documentation files (the 'Software') for", 
      "\n#' non-commercial academic use. The Software is restricted to only non-commercial",
      "\n#' academic use and cannot be modified, merged, published, distributed, sublicensed,", 
      sprintf("\n#' or sold without the written permission of a representative of the %s.", copyright_holder)
    )
  } else{
    writeLines(c)
  }
  
}

