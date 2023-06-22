#' @title use_ku_license
#' @description Creates a roxygen2 style skeleton for standard license text for the University of Kansas Medical Center
#' @param year PARAM_DESCRIPTION, Default: format(Sys.Date(), "%Y")
#' @param copyright_holder PARAM_DESCRIPTION, Default: NULL
#' @return Prints out a roxygen2 style skeleton in the R console for standard license text for the University of Kansas Medical Center
#' @details Creates a roxygen2 style skeleton for standard license text for the University of Kansas Medical Center
#' @rdname use_ku_license
#' @export 

use_ku_license <- function(year = format(Sys.Date(), "%Y"), copyright_holder = NULL){
  
  if(is.null(copyright_holder)) copyright_holder <- "University of Kansas Medical Center"
  
  c1 <- sprintf(paste0("#' ", "Copyright %s" ), year)
  c2 <- sprintf(paste0("#' ", "Copyright Holder: %s"), copyright_holder)
  c3 <- paste0("#' ", "All rights reserved.")
    
  c <- sprintf("%s\n%s\n%s", c1, c2, c3)
  
  writeLines(c)
}



