#' @title Parvo Extract Meta Data
#' @description Extracts the Parvo meta data from the top of the XLSX file.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @return Returns a vector with the stored meta data from the Parvo XLSX file.
#' @details Extracts the Parvo meta data from the top of the XLSX file.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname parvo.extract.meta
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_replace

# Extract Meta Data
parvo.extract.meta <- function (parvo.path) {
  id <- substr(basename(parvo.path), 1, 4)
  file <- readxl::read_xlsx(parvo.path, col_names = c(paste0("Col", 1:12)))
  meta <- file[1:26,]
  location <- paste0(meta[1, as.vector(!is.na(meta[1, ]))==TRUE], " ", meta[2, as.vector(!is.na(meta[1, ]))==TRUE])
  starttime <- as.POSIXct(paste0(meta[3, 2], "/", meta[3, 4], "/", meta[3, 6], " ", meta[3, 7], ":", meta[3,9], ":", meta[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  name <- paste0(unlist(strsplit(stringr::str_replace(as.character(meta[6,2]), " ", ""), "[,]"))[2], " ", unlist(strsplit(stringr::str_replace(as.character(meta[6,2]), " ", ""), "[,]"))[1])
  age <- as.numeric(meta[7, 2])
  gender <- paste0(meta[7, 5])
  height_in <- paste0(meta[8,2])
  weight_lbs <- round((as.numeric(meta[8,7])), 2)
  room_temp <- as.numeric(paste0(meta[16,2]))*(9/5) + 32
  baro_pres <- round(as.numeric(paste0(meta[16,5])), 2)
  humidity <- round((as.numeric(paste0(meta[17,4]))*100), 2)
  demo <- cbind(id = id, location, starttime = as.character(starttime), name, age, gender, height_in, weight_lbs, room_temp, baro_pres, humidity)
  return(demo)
}

#' @title Parvo Extract Data
#' @description Extracts the Parvo data from the XLSX file.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @param ree Logical value indicating whether you are extracting data from a resting energy expenditure measurement, Default: FALSE
#' @param aee Logical value indicating whether you are extracting data from a activity energy expenditure measurement, Default: FALSE
#' @param time.breaks Time value (e.g., 5 sec, 1 min) to indicate the interval for which the data should be aggregated, Default: '1 sec'
#' @return Returns a data set with the Parvo data.
#' @details Extracts the Parvo data from the XLSX file.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[lubridate]{as_date}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}}
#' @rdname parvo.extract.data
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom lubridate as_datetime

# Extract Observations from VO2 Max Test
parvo.extract.data <- function (parvo.path, ree=FALSE, aee=FALSE, time.breaks = "1 sec") {
  file <- readxl::read_xlsx(parvo.path, col_names = c(paste0("Col", 1:12)))
  starttime <- as.POSIXct(paste0(file[3, 2], "/", file[3, 4], "/", file[3, 6], " ", file[3, 7], ":", file[3,9], ":", file[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  
  if (ree==TRUE){
    vo2 <- as.data.frame(file[32:nrow(file), 1:10])
    colnames(vo2) <- c("time.min", "vo2.ml.min", "vo2.ml.kg.min", "mets", "vco2.ml.min", "ve.l.min", "rq", "feo2%", "feco2%", "ree.kcal.d")
    vo2 <- vo2[is.na(vo2$ree.kcal.d)==FALSE, ]
  }
  
  if(aee==TRUE){
    vo2 <- as.data.frame(file[32:nrow(file), 1:9])
    colnames(vo2) <- c("time.min", "vo2.l.min", "vo2.ml.kg.min", "mets", "rer", "ree.kcal.min", "tm.per.grade", "tm.speed", "ve.l.min")
    vo2 <- vo2[is.na(vo2$tm.speed)==FALSE & vo2$tm.speed!=0, ]
  }
  
  vo2 <- cbind(datetime = (starttime + (as.numeric(vo2$time.min)*60)), vo2)
  #first_record = lubridate::as_datetime(paste0(vo2$date[1], " ", vo2$time[1]), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S")
  #last_record = lubridate::as_datetime(paste0(vo2$date[nrow(vo2)], " ", vo2$time[nrow(vo2)]), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S")
  vo2[names(dplyr::select(vo2, 3:ncol(vo2)))] <- round(sapply(vo2[names(dplyr::select(vo2, 3:ncol(vo2)))], as.numeric), 3)
  `%>%` <- dplyr::`%>%`
  
  n.obs <- vo2 %>% dplyr::group_by(time = format(vo2$datetime, format = "%H:%M")) %>% dplyr::summarise(n = dplyr::n())
  n.obs <- as.vector(n.obs[n.obs$n<6, "time"])
  vo2 <- vo2[!format(vo2$datetime, format="%H:%M") %in% n.obs, ] # Exclude first or last observations if they have fewer than 6 breaths
  
  vo2 <- vo2 %>%
    dplyr::group_by(timestamp = cut(vo2$datetime, breaks = time.breaks)) %>%
    dplyr::summarise_at(c(names(dplyr::select(vo2, 3:ncol(vo2)))), mean) %>%
    dplyr::ungroup()
  vo2$timestamp <- as.POSIXct(strftime(as.character(vo2$timestamp), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S"))
  return(vo2)
}

#' @title Resting Energy Expenditure Main Function
#' @description Read in Parvo and Accelerometer data (if available) and calculate the estimated resting energy expenditure.
#' @param accel.path Pathname to the accelerometer AGD file, Default: NULL.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @param var.limit Number to ensure changes in ventilation, oxygen consumption, and respiratory quotient do not exceed variation limit.
#' @return Returns a matrix with the resting energy expenditure output, the length of time of steady state, and the number of the last observations used (maximum: 5).
#' @details Read in Parvo and Accelerometer data (if available) and calculate the estimated resting energy expenditure.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[lubridate]{period}},\code{\link[lubridate]{round_date}}
#'  \code{\link[dplyr]{lead-lag}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{rename}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{context}},\code{\link[dplyr]{select}}
#'  \code{\link[bhelselR]{read_agd}}
#' @rdname parvo.ree.main
#' @export 
#' @importFrom lubridate minutes round_date
#' @importFrom stats na.omit

parvo.ree.main <- function(accel.path = NULL, parvo.path, var.limit = 15) {
  data <- bhelselR::parvo.extract.data(parvo.path, ree=TRUE, time.breaks = "1 min")
  data <- data[data$timestamp > min(data$timestamp) + lubridate::minutes(14), ]
  `%>%` <- dplyr::`%>%`
  
  data$diff.ve.l.min <- (((data$ve.l.min-dplyr::lag(data$ve.l.min, 1))/dplyr::lag(data$ve.l.min, 1))*100)
  data$diff.vo2.ml.kg.min <- (((data$vo2.ml.kg.min-dplyr::lag(data$vo2.ml.kg.min, 1))/dplyr::lag(data$vo2.ml.kg.min, 1))*100)
  data$diff.rq <- (((data$rq-dplyr::lag(data$rq, 1))/dplyr::lag(data$rq, 1))*100)
  
  data <- data %>% stats::na.omit() %>%
    dplyr::filter(abs(diff.ve.l.min)<var.limit & abs(diff.vo2.ml.kg.min)<var.limit & abs(diff.rq)<var.limit)
  
  
  if(is.null(accel.path)==FALSE){
    accel <- bhelselR::read_agd(accel.path)
    accel <- cbind(timestamp = paste(accel$Date, accel$` Time`), accel)
    accel[, c("Date", " Time")] <- NULL
    accel$timestamp <- as.POSIXct(strptime(accel$timestamp, tz = Sys.timezone(), format = "%m/%d/%Y %H:%M:%S"))
    accel$timestamp <- lubridate::round_date(accel$timestamp, unit = "1 minute")
    accel <- accel[, c("timestamp", " Axis1", "HR")]
    accel <- dplyr::rename(accel, "accel.hr.bpm" = "HR", "counts" = " Axis1")
    accel <- accel %>%
      dplyr::group_by(timestamp = cut(accel$timestamp, breaks = "1 min")) %>%
      dplyr::summarise_all(mean) %>%
      dplyr::ungroup()
    accel$timestamp <- as.POSIXct(strftime(as.character(accel$timestamp), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S"))
    data <- merge(data, accel, by="timestamp", all.x=TRUE)
    data <- data[data$counts<50, ]
  }
  
  data$time.group <- cumsum(c(TRUE, diff(data$timestamp)>1))
  data <- data %>% dplyr::group_by(time.group) %>% dplyr::mutate(steady.state.minutes = dplyr::n())
  
  data <- data[data$steady.state.minutes==max(data$steady.state.minutes), ] # Return Max
  
  if(length(unique(data$time.group))>1){
    data <- data[data$time.group==max(data$time.group), ] # Return last steady state for periods of equivalent length
  }
    
  data <- data[(nrow(data)-4):nrow(data), ] # Return last 5 if max is greater than 5 minutes.
  data$n.obs <- nrow(data) # Number of observations used.
  
  data <- data %>%
    dplyr::select(-c(timestamp, diff.ve.l.min, diff.vo2.ml.kg.min, diff.rq)) %>%
    dplyr::group_by(time.group) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup()
  data$mod.ml.kg.min <- data$vo2.ml.kg.min*3
  data$vig.ml.kg.min <- data$vo2.ml.kg.min*6
  data <- data[, names(dplyr::select(data, -c(time.group)))]
  return(t(data))
}

#' @title Parvo AEE Final 4
#' @description Takes average of last 4 minutes of the walking protocol from the WalkDS study.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @param corrected.time.path Pathname to a document of corrected time stamps, Default: NULL.
#' @param accel.path Pathname to the accelerometer AGD file, Default: NULL.
#' @param rest1met Resting VO2 for 1 metabolic equivalent (MET), Default = 3.5 ml/kg/min
#' @param return_raw_data Return raw data aggregated to 5-second epochs instead of a minute-level summary
#' @param epoch_size Epoch size for agcounts package to calculate counts, Default: 5
#' @param minutes_returned Number of minutes of data returned (range 1-7 minutes), Default: 4
#' @return Returns an average for the last 4 minutes of the WalkDS walking stages for VO2, METS, and RQ.
#' @details Takes average of last 4 minutes of the walking protocol from the WalkDS study.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{select}}
#' @rdname parvo.aee.final4
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom lubridate force_tz round_date
#' @importFrom agcounts get_counts


parvo.aee.final4 <- function (parvo.path, corrected.time.path = NULL, accel.path = NULL, 
                              rest1met = 3.5, return_raw_data = FALSE, epoch_size = 5,
                              minutes_returned = 4) {
  file <- readxl::read_xlsx(parvo.path, col_names = c(paste0("Col", 1:12)))
  id <- strsplit(x = gsub(pattern = ".xlsx", replacement = "", x = basename(parvo.path)), split = "S")[[1]][1]
  stage <- strsplit(x = gsub(pattern = ".xlsx", replacement = "", x = basename(parvo.path)), split = "S")[[1]][2]
  
  if(is.null(corrected.time.path)){
    starttime <- as.POSIXct(paste0(file[3, 2], "/", file[3, 4], "/", file[3, 6], " ", file[3, 7], ":", file[3,9], ":", file[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())  
  }
  
  if(!is.null(corrected.time.path)){
    corrected.time <- readxl::read_xlsx(corrected.time.path)
    corrected.time <- corrected.time[, c("id", "stage", "start")]
    starttime <- corrected.time[corrected.time$id==as.numeric(id) & corrected.time$stage==as.numeric(stage), "start"][[1]]
    starttime <- strsplit(x = starttime, split = "Start: ")[[1]][2]
    starttime <- as.POSIXct(starttime)
  }
  
  vo2 <- as.data.frame(file[32:nrow(file), 1:9])
  colnames(vo2) <- c("time.min", "vo2.l.min", "vo2.ml.kg.min", "mets", "rer", "ree.kcal.min", "tm.per.grade", "tm.speed", "ve.l.min")
  vo2 <- vo2[is.na(vo2$tm.speed)==FALSE & vo2$tm.speed!=0, ]
  vo2[names(dplyr::select(vo2, time.min:ve.l.min))] <- sapply(vo2[names(dplyr::select(vo2, time.min:ve.l.min))], as.numeric)
  vo2$mets <- vo2$vo2.ml.kg.min / rest1met
  vo2 <- vo2[vo2$time.min >= (7.5 - as.integer(minutes_returned)) & vo2$time.min <= 7.5, ]
  vo2 <- cbind(timestamp = format(starttime + as.numeric(vo2$time.min)*60, "%Y/%m/%d %H:%M:%S"),  vo2)
  vo2$timestamp <- as.POSIXct(vo2$timestamp, format = "%Y/%m/%d %H:%M:%S") %>% lubridate::force_tz("UTC")
  vo2.summary <- rbind(paste0("Start Time: ", starttime), paste0("VO2 L/min: ", round(mean(vo2$vo2.l.min), 3)),
                       paste0("VO2/kg: ", round(mean(vo2$vo2.ml.kg.min), 3)), paste0("METS: ", round(mean(vo2$mets), 3)), paste0("RQ: ", round(mean(vo2$rer), 3)))
  
  if(!is.null(accel.path)){
    gt3x.files <- list.files(accel.path, pattern = ".gt3x$", full.names = TRUE)
    agd.files <- list.files(accel.path, pattern = ".agd$", full.names = TRUE)
    hip <- get_counts(path = gt3x.files[grep(paste0("BH", id), gt3x.files)], epoch = epoch_size, parser = "gt3x")
    hip <- hip[c("time", "Axis1", "Vector.Magnitude")]
    hip <- hip %>% dplyr::filter(time >= vo2$timestamp[1] & time <= vo2$timestamp[nrow(vo2)])
    
    hip <- 
      read_agd(agd.files[grep("BH", agd.files)]) %>%
      mutate(time = as.POSIXct(paste0(Date, " ", ` Time`),
                                    format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
             time = lubridate::round_date(time, unit = paste(epoch_size, "second"))) %>%
      group_by(time) %>%
      summarise(HR = round(mean(HR, na.rm = TRUE))) %>%
      mutate(HR = ifelse(HR == 0, NA, HR)) %>%
      dplyr::filter(time >= vo2$timestamp[1] & time <= vo2$timestamp[nrow(vo2)]) %>%
      merge(x = hip, y = ., by = "time")
      
    hip.summary <- 
      hip %>% 
      dplyr::group_by(time = lubridate::round_date(time, unit="1 minute")) %>% 
      dplyr::summarise(Axis1 = sum(Axis1), HR = mean(HR, na.rm = TRUE), Vector.Magnitude = sum(Vector.Magnitude), n = dplyr::n()) %>%
      mutate(epoch_size = epoch_size)
    
    
    right.wrist <- get_counts(path = gt3x.files[grep(paste0("R", id), gt3x.files)], epoch = epoch_size, parser = "gt3x")
    right.wrist <- right.wrist[c("time", "Axis1", "Vector.Magnitude")]
    right.wrist <- right.wrist %>% dplyr::filter(time >= vo2$timestamp[1] & time <= vo2$timestamp[nrow(vo2)])
    
    right.wrist.summary <- 
      right.wrist %>% 
      dplyr::group_by(time = lubridate::round_date(time, unit="1 minute")) %>% 
      dplyr::summarise(Axis1 = sum(Axis1), Vector.Magnitude = sum(Vector.Magnitude), n = dplyr::n()) %>%
      mutate(epoch_size = epoch_size)
  
    left.wrist <- get_counts(path = gt3x.files[grep(paste0("L", id), gt3x.files)], epoch = epoch_size, parser = "gt3x")
    left.wrist <- left.wrist[c("time", "Axis1", "Vector.Magnitude")]
    left.wrist <- left.wrist %>% dplyr::filter(time >= vo2$timestamp[1] & time <= vo2$timestamp[nrow(vo2)])
  
    left.wrist.summary <- 
      left.wrist %>% 
      dplyr::group_by(time = lubridate::round_date(time, unit="1 minute")) %>% 
      dplyr::summarise(Axis1 = sum(Axis1), Vector.Magnitude = sum(Vector.Magnitude), n = dplyr::n()) %>%
      mutate(epoch_size = epoch_size)
    
    accel.summary <- rbind(paste0("HR: ", round(mean(hip.summary$HR), 1)), 
                           paste0("Hip Vertical Axis: ", round(mean(hip.summary$Axis1), 1)),
                           paste0("Hip Vector Magnitude: ", round(mean(hip.summary$Vector.Magnitude), 1)), 
                           paste0("Right Wrist Vertical Axis: ", round(mean(right.wrist.summary$Axis1), 1)),
                           paste0("Right Wrist Vector Magnitude: ", round(mean(right.wrist.summary$Vector.Magnitude), 1)), 
                           paste0("Left Wrist Vertical Axis: ", round(mean(left.wrist.summary$Axis1), 1)),
                           paste0("Left Wrist Vector Magnitude: ", round(mean(left.wrist.summary$Vector.Magnitude), 1)))
  }
  
  if(return_raw_data){
    vo2 <- 
      vo2 %>%
      mutate(time = lubridate::round_date(vo2$timestamp, unit = paste(epoch_size, "second"))) %>%
      select(time, vo2.l.min:ve.l.min) %>%
      group_by(time) %>%
      summarise_all(mean, na.rm = TRUE)
    
    hip %<>% rename("hip.va" = "Axis1", "hip.vm" = "Vector.Magnitude", "heart.rate" = "HR")
    
    right.wrist %<>% rename("rwrist.va" = "Axis1", "rwrist.vm" = "Vector.Magnitude")
    
    left.wrist %<>% rename("lwrist.va" = "Axis1", "lwrist.vm" = "Vector.Magnitude")
    
    data <- vo2 %>% 
      merge(., hip, by = "time") %>% 
      merge(., right.wrist, by = "time") %>% 
      merge(., left.wrist, by = "time")
    
    return(data)
  }
  
  if(!return_raw_data){
    if(!is.null(accel.path)) {
      return(rbind(vo2.summary, accel.summary))
    }
    
    if(is.null(accel.path)) {
      return(vo2.summary)
    }
  }
}


