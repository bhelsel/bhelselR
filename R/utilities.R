# Copyright © 2021 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Sort Files in Downloads Folder
#' @description This function allows a user to clean up their downloads folder.
#' @param allowed_filetypes Add file types, Default: NULL
#' @return A sorted downloads folder 
#' @details This function allows a user to clean up their downloads folder. By
#' changing the allowed file types, a user can specify additional files that 
#' they want to keep in the downloads folder.
#' @examples sort_files_in_downloads(allowed_filetypes = c("zip", "txt", "R"))
#' @seealso 
#'  \code{\link[tools]{fileutils}}
#' @rdname sort_files_in_downloads
#' @export 
#' @importFrom tools file_ext

sort_files_in_downloads <- function(allowed_filetypes = NULL){
  
  allowed_files <- c("docx", "pdf", "pptx", "html", "mov", "jpeg", 
                     "jpg", "png", "xls", "xlsx", "doc", "csv")
  
  if(!is.null(allowed_filetypes)){
    
    allowed_files <- c(allowed_files, allowed_filetypes)
    
  }
  
  # Sort and clean the documents in my downloads folder 
  
  files <- list.files("~/Downloads", full.names = TRUE)
  
  # Create folders from allowed file types
  ext <- unique(tools::file_ext(files))
  ext <- ext[ext %in% allowed_files]
  invisible(sapply(paste0("~/Downloads", "/", ext), function(x) if(!dir.exists(x)) dir.create(x)))
  
  # Prepare and move files from allowed file types
  files2move <- files[grepl("[.]", files) & tools::file_ext(files) %in% allowed_files]
  newpaths <- paste0(dirname(files2move), "/", tools::file_ext(files2move), "/", basename(files2move))
  invisible(file.copy(from = files2move, to = newpaths))
  invisible(file.remove(files2move))
  
  # Remove files that are not in allowed file types
  files2delete <- files[grepl("[.]", files) & !files %in% files2move]
  file.remove(files2delete)
  
}