#' @title gt3x2csv
#' @description Convert a single GT3X file to a CSV file with removed timestamp to be processed by GGIR.
#' @param origin Origin of the GT3X file.
#' @param output Desired location of the CSV file.
#' @param studyname Study name or abbreviation to be used in the naming of the file.
#' @return Returns a CSV file to the path name specified under output.
#' @details Convert a single GT3X file to a CSV file with removed timestamp to be processed by GGIR.
#' @examples 
#' \dontrun{
#'  #Add example here.
#' }
#' @rdname gt3x2csv
#' @export 

library(read.gt3x)
library(readr)

gt3x2csv <- function(origin, output, studyname){
  info <- parse_gt3x_info(origin)
  df <- read.gt3x(origin, asDataFrame = TRUE, imputeZeroes = TRUE)
  df$time <- NULL
  id <- info$`Subject Name`
  colnames(df) <- c("Accelerometer X", "Accelerometer Y", "Accelerometer Z")
  header1 <- "------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.4 Firmware v3.2.1 date format yyyy.MM.dd at 60 Hz  Filter Normal -----------"
  header2 <- paste0("Serial Number: ", info$`Serial Number`)
  header3 <- paste0("Start Time ", format(info$`Start Date`, "%H:%M:%S"))
  header4 <- paste0("Start Date ", format(info$`Start Date`, "%Y.%m.%d"))
  header5 <- "Epoch Period (hh:mm:ss) 00:00:00"
  header6 <- paste0("Download Time ", format(info$`Download Date`, "%H:%M:%S"))
  header7 <- paste0("Download Date ", format(info$`Download Date`, "%Y.%m.%d"))
  header8 <- "Current Memory Address: 0"
  header9 <- paste0("Current Battery Voltage: ", info$`Battery Voltage`,"     Mode = 12")
  header10 <- "--------------------------------------------------"
  header_txt <- data.frame(header = rbind(header1, header2, header3, header4, header5, header6, header7, header8, header9, header10))
  write_csv(header_txt, paste0(output, "/", id, "_", studyname, ".csv"), append = FALSE, col_names = FALSE)
  write_csv(df, paste0(output, "/", id, "_", studyname, ".csv"), append = TRUE, col_names = TRUE)
}

