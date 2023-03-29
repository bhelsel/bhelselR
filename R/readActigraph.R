#' @title Convert GT3X Files to CSV
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
#' @importFrom data.table fwrite

gt3x2csv <- function(origin, output, studyname){
  info <- read.gt3x::parse_gt3x_info(origin)
  df <- read.gt3x::read.gt3x(origin, asDataFrame = TRUE, imputeZeroes = TRUE)
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
  data.table::fwrite(header_txt, paste0(output, "/", id, "_", studyname, ".csv"), append = FALSE, col.names = FALSE)
  data.table::fwrite(df, paste0(output, "/", id, "_", studyname, ".csv"), append = TRUE, col.names = TRUE)
}

#' @title Read ActiGraph AGD Files
#' @description Read the settings or data from the ActiGraph AGD Files
#' @param agd_filename The path to the AGD file or the filename if the AGD file is in the current working directory.
#' @param settings Returns the AGD file meta data that is reported in ActiGraph CSV Files in a data frame, Default: FALSE
#' @param data Returns the AGD file accelerometer data in a data frame, Default: TRUE
#' @return Returns a data frame or a list if settings and data are both TRUE
#' @details Read the settings or data from the ActiGraph AGD Files
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[DBI]{dbConnect}},\code{\link[DBI]{dbReadTable}},\code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname read_agd
#' @export 
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom utils tail

read_agd <- function(agd_filename, settings=FALSE, data=TRUE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), agd_filename)
  if (settings==TRUE){
    agd_set <- DBI::dbReadTable(con, "settings")
    get_value <- function(settingName) {agd_set[agd_set["settingName"]==settingName, "settingValue"]}
    get_time <- function(settingName) {as.POSIXlt((as.numeric(agd_set[agd_set["settingName"]==settingName, "settingValue"]) / 1e7), origin = "0001-01-01 00:00:00", tz = "GMT")}
    start_datetime <- get_time("startdatetime")
    start_date <- format(start_datetime, "%m-%d-%Y")
    start_time <- strftime(start_datetime, format = "%H:%M:%S")
    download_datetime = get_time("downloaddatetime")
    download_date <- format(download_datetime, "%m-%d-%Y")
    download_time <- strftime(download_datetime, format = "%H:%M:%S")
    epochlength <- strftime(as.POSIXlt(as.numeric(get_value("epochlength")), origin = "0001-01-01 00:00:00", tz="GMT"), format = "%H:%M:%S")
    r1 = paste0("------------ Data Table File Created By Actigraph", " ", get_value("devicename"), " ", get_value("softwarename"), " v", get_value("softwareversion"), " ", "date format", " ", get_value("datetimeformat"), " ", "Filter", " ", get_value("filter"), " -----------")
    r2 = paste0("Serial Number:", " ", get_value("deviceserial"))
    r3 = paste0("Start Time", " ", start_time)
    r4 = paste0("Start Date", " ", start_date)
    r5 = paste0("Epoch Length (hh:mm:ss)", " ", epochlength)
    r6 = paste0("Download Time", " ", download_time)
    r7 = paste0("Download Date", " ", download_date)
    r8 = paste0("Current Memory Address:", " ", "0")
    r9 = paste0("Current Battery Voltage:", " ", get_value("batteryvoltage"), "     ", "Mode", " = ", get_value("modenumber"))
    r10 = paste0("--------------------------------------------------")
    agd_set = data.frame(rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10))
  }
  if (data==TRUE){
    agd_data <- DBI::dbReadTable(con, "data")
    agd_data$dataTimestamp <- format(as.POSIXlt((agd_data$dataTimestamp / 1e7), origin = "0001-01-01 00:00:00", tz = "GMT"), "%Y-%m-%d %H:%M:%S")
    agd_data$Date <- format(as.Date(agd_data$dataTimestamp), "%m/%d/%Y")
    agd_data$Time <- format(as.POSIXct(agd_data$dataTimestamp, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()), "%H:%M:%S")
    agd_data$dataTimestamp <- NULL
    
    namekey <- c(Date = "Date", Time = " Time", axis1 = " Axis1", axis2 = "Axis2", axis3 = "Axis3", steps = "Steps", 
                 hr = "HR", lux = "Lux", inclineOff = "Inclinometer Off", inclineStanding = "Inclinometer Standing", 
                 inclineSitting = "Inclinometer Sitting", inclineLying = "Inclinometer Lying")
    names(agd_data) <- namekey[names(agd_data)]
    agd_data <- cbind(agd_data[, utils::tail(names(agd_data), 2)], agd_data)
    agd_data[, c(ncol(agd_data)-1, ncol(agd_data))] <- NULL
    if(" Axis1" %in% names(agd_data) & "Axis2" %in% names(agd_data) & "Axis3" %in% names(agd_data)) {
      agd_data$`Vector Magnitude` <- round((sqrt(agd_data$` Axis1`^2 + agd_data$Axis2^2 + agd_data$Axis3^2)))
    }
  }
  DBI::dbDisconnect(con)
  if (settings==TRUE & data==TRUE) {
    return(list(agd_set, agd_data))
  } else if (settings==TRUE & data==FALSE) {
    return(data.frame(agd_set, check.names = FALSE))
  } else if (settings==FALSE & data==TRUE) {
    return(data.frame(agd_data, check.names = FALSE))
  }
}


#' @title Convert AGD to CSV Files
#' @description Converts ActiGraph AGD to CSV Files
#' @param data_directory Location of the folder where the ActiGraph
#' @return Returns a CSV folder in the data directory with the ActiGraph CSV Files
#' @details Converts ActiGraph AGD to CSV Files
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readr]{write_delim}}
#' @rdname agd_to_csv
#' @export 
#' @importFrom readr write_csv

agd_to_csv <- function(data_directory) {
  start_time <- Sys.time()
  setwd(data_directory)
  newdatadir <- paste0(data_directory, "/CSV")
  if (dir.exists(newdatadir)==FALSE) {
    dir.create(newdatadir)
  }
  files <- sort(list.files(data_directory))
  agd_files <- grep(".agd", files, value=TRUE)
  no.files <- length(list.files(newdatadir))
  for (file in agd_files) {
    if (file.exists(paste0(newdatadir, "/", gsub(".agd", ".csv", file)))==FALSE){
      new_filename <- gsub(".agd", ".csv", file)
      agd_settings <- read_agd(paste0(data_directory, "/", file), settings=TRUE, data=FALSE)
      agd_data <- read_agd(paste0(data_directory, "/", file), settings=FALSE, data=TRUE)
      readr::write_csv(agd_settings, paste0(newdatadir, "/", new_filename), append = FALSE, col_names = FALSE)
      readr::write_csv(agd_data, paste0(newdatadir, "/", new_filename), append = TRUE, col_names = TRUE)
    }
  }
  no.files <- length(list.files(newdatadir)) - no.files
  end_time <- Sys.time()
  paste0("All done! This accelerometer program took ", round(((end_time - start_time)), 2), " seconds to convert ", no.files, " records")
  return(newdatadir)
}
