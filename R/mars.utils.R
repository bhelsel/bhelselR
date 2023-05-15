#' @title birth.date
#' @description Reads in a date of birth csv file located in the same data folder as the accelerometer files to apply age-specific cutpoints.
#' @param datadir Data directory for the accelerometer files.
#' @param files Files to search through for the date of birth CSV
#' @return Returns a date of birth data set that can be applied to the accelerometer data for age-specific cutpoints.
#' @details Reads in a date of birth csv file located in the same data folder as the accelerometer files to apply age-specific cutpoints.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readr]{read_delim}}
#' @rdname birth.date
#' @export 
#' @importFrom utils read.csv
#' @importFrom lubridate parse_date_time
#' @importFrom stats complete.cases
#' @importFrom rlang is_empty

birth.date <- function(datadir, files){
  dob.file <- grep("dob", files, value=TRUE)
  if(rlang::is_empty(dob.file)){
    stop("No date of birth file found")
  }
  dob <- utils::read.csv(paste0(datadir, "/", dob.file))
  dob <- dob[, c(1:2)]
  dob <- dob[stats::complete.cases(dob), ]
  dob$id <- as.character(dob$id)
  colnames(dob) <- tolower(colnames(dob))
  
  year.length <- nchar(strsplit(dob$dob, "/")[[1]][3])
  
  if(year.length==4){
    dob$dob <- as.Date(dob$dob, tryFormats = c("%m/%d/%Y", "%m/%d/%y"))
  } else{
    dob$dob <- as.Date(dob$dob, tryFormats = c("%m/%d/%y", "%m/%d/%Y"))
  }
  
  return(dob)
}


#' @title AGread.csv
#' @description Reads in the accelerometer csv file and prepares the data set for processing. 
#' @param demo Date of birth file that will be matched based on recrod id to apply age-specific cutpoints, Default: NULL
#' @param newdatadir Data directory where the CSV file is stored
#' @param file Location of the file to be processed. 
#' @param record_id Participant ID.
#' @return Returns a data set with the timepoint (e.g., A=Baseline, B=6-months, etc.), record ID, timestamp, age of the participant (if date of birth file is used), vertical axis counts, and vector magnitude.
#' @details Reads in the accelerometer csv file and prepares the data set for processing. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readr]{read_delim}}
#' @rdname AGread.csv
#' @export 
#' @importFrom utils read.csv
#' @importFrom stats complete.cases

AGread.csv <- function(demo=NULL, newdatadir, file, record_id){
  data <- utils::read.csv(paste0(newdatadir, "/", file), skip = 10)
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- make.names(colnames(data))
  
  year.length <- nchar(strsplit(data$date, "/")[[1]][3])
  if(year.length==4){
    data$date <- as.Date(data$date, tryFormats = c("%m/%d/%Y", "%m/%d/%y"))
  } else{
    data$date <- as.Date(data$date, tryFormats = c("%m/%d/%y", "%m/%d/%Y"))
  }
  
  data$record.id <- record_id
  data$time.stamp <- as.POSIXct(paste0(data$date, " ", data$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  `%notin%` <- Negate(`%in%`)
  `%>%` <- dplyr::`%>%`
  
  if(is.null(demo)==FALSE){
    data$dob <- demo[demo$id==substring(record_id, 2, nchar(record_id)), "dob"]
    data$age <- as.integer(difftime(data$date, data$dob, units = "days") / 365.25)
    data$age <- data$age[1]
    data$dob <- NULL
  }
  
  data$counts = data$axis1
  
  if("vector.magnitude" %in% colnames(data)) {
    data <- data %>% dplyr::relocate(c(vector.magnitude, steps), .after = counts) %>% dplyr::select(record.id:steps)
    data <- data[stats::complete.cases(data), ]
  }
  
  if("vector.magnitude" %notin% colnames(data)){
    data <- data %>% dplyr::relocate(steps, .after = counts) %>% dplyr::select(record.id:steps)
    data <- data[stats::complete.cases(data), ]
  }
  return(data)
}

#' @title AG.temporal
#' @description Creates temporal variables from the timestamp that can include season, weekday, and time of the day.
#' @param data Accelerometer data set with a time.stamp variable.
#' @param season Boolean value that will return winter, spring, summer, or fall if TRUE, Default: TRUE
#' @param weekday Boolean value that will return the day of the week from time.stamp if TRUE, Default: FALSE
#' @param time Boolean value that will group the time.stamp into specified categories if TRUE, Default: FALSE
#' @param time.breaks Integer values using the 24 hour clock, Default: c(0, 9, 15, 19, 24)
#' @param time.labels Labels to be returned from the time.breaks (must be one less value than time.breaks), Default: c("12-9 am", "9 am-3 pm", "3-7 pm", "After 7 pm")
#' @return Returns a data set with the added weekday, time, and season values.
#' @details Creates temporal variables from the timestamp that can include season, weekday, and time of the day.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[lubridate]{day}}
#'  \code{\link[tibble]{add_column}}
#' @rdname AG.temporal
#' @export 
#' @importFrom lubridate yday
#' @importFrom tibble add_column

AG.temporal <- function(data, season=TRUE, weekday=FALSE, time=FALSE,
                        time.breaks = c(0, 9, 15, 19, 24),
                        time.labels = c("12-9 am", "9 am-3 pm", "3-7 pm", "After 7 pm")) {
  
  if(season==TRUE) {
    
    season <- cut(lubridate::yday(data$time.stamp), breaks = c(0, 78, 171, 265, 355, 366), 
                  labels = c("Winter", "Spring", "Summer", "Fall", "Winter"), include.lowest=TRUE, right=FALSE)
    
    data <- tibble::add_column(data, season, .after="time.stamp")
    
    }
  
  if(time==TRUE) {
    
    time.category <- cut(as.numeric(strftime(data$time.stamp, "%H", tz = "UTC")), 
                         breaks = time.breaks, labels = time.labels, 
                         right=FALSE, include.lowest=TRUE)
    
    data <- tibble::add_column(data, time.category, .after="time.stamp")
    
    }
  
  if(weekday==TRUE) {
    
    weekday <- strftime(data$time.stamp, "%A")
    
    data <- tibble::add_column(data, weekday, .after="time.stamp")
    
  }
  
  return(data)
  
}






