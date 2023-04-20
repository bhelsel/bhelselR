#' @title parvo_get_oues
#' @description A function to calculate Oxygen Uptake Efficiency Slope at 75%, 90%, and 100% VO2 max measured using a Parvo Medics TrueOne 2400 metabolic cart
#' @param path Path to a single file or directory with the VO2 Max excel files
#' @param return.data Returns the OUES results to the R console, Default: TRUE
#' @param write.summary.file Exports a summary CSV file of VO2 max characteristics for study participants, Default: FALSE
#' @param write.individual.file Exports individual CSV files for study participants, Default: FALSE
#' @param verbose Print the progress of the OUES calculation, Default: FALSE
#' @param ... arguments passed to \code{\link[data.table]{fwrite}}
#' @return Returns the OUES results or writes individual or summary files
#' @details A function to calculate Oxygen Uptake Efficiency Slope at 75%, 90%, and 100% VO2 max measured using a Parvo Medics TrueOne 2400 metabolic cart
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  parvo_get_oues(path)
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[data.table]{fwrite}}
#'  \code{\link[plyr]{rbind.fill}}
#'  \code{\link[stats]{lm}}
#' @rdname parvo_get_oues
#' @export 
#' @importFrom stringr str_split
#' @importFrom readxl read_xlsx
#' @importFrom data.table fwrite
#' @importFrom plyr rbind.fill
#' @importFrom stats lm

parvo_get_oues <- function(path, return.data = TRUE, write.summary.file = FALSE, write.individual.file = FALSE, verbose = FALSE, ...){
  
  if(dir.exists(path)){
    files <- list.files(path, full.names = TRUE, pattern = ".xls$|.xlsx$")
  } else {
    files <- path
  }
  
  main.data <- data.frame()
  
  for(i in 1:length(files)){
    d <- dirname(files[i])
    b <- stringr::str_split(basename(files[i]), "_")[[1]]
    id <- paste0(substr(b[2], 1, 1), ".", substr(b[1], 1, 4))
    data <- readxl::read_xlsx(files[i], col_names = c(paste0("Col", 1:12)))
    parvo_data <- .parvo_calculate_oues(data)
    colnames(parvo_data[[1]]) <- c("oues75", "oues90", "oues100")
    
    vo2max <- cbind(parvo_data[[2]][which(parvo_data[[2]]$event=="vo2.max.100"), 2:13], parvo_data[[1]])
    
    parvo_settings <- data.frame(
      rbind(
        paste("------------ Data Table Created By R from an Exported Parvo Medics TrueOne 2400 Metabolic Cart on", format(Sys.Date(), "%B %d, %Y"), "-----------"),
        "", "TEST ENVIRONMENT", .parvo_extract_vo2_max_test_env(data), "", "PARTICIPANT CHARACTERISTICS", .parvo_extract_vo2_max_participant_info(data), "",
        cbind(sapply(1:4, function(x) paste(.parvo_extract_vo2_max_meta(data)[[1]][x, 1], .parvo_extract_vo2_max_meta(data)[[1]][x, 2]))), "",
        cbind(sapply(1:3, function(x) paste(.parvo_extract_vo2_max_meta(data)[[2]][x, 1], round(as.numeric(.parvo_extract_vo2_max_meta(data)[[2]][x, 2]), 3)))),
        paste("Time to Max:", round(vo2max[1], 3), "minutes"), paste0("OUES 75: ", round(vo2max$oues75, 4), "; OUES 90: ", round(vo2max$oues90, 4), "; OUES 100: ", round(vo2max$oues100, 4)),
        paste0("--------------------------------------------------")
      )
    )
    
    new_path <- paste0(d, "/", gsub(".xls$|.xlsx$", "_OUES.csv", basename(files[i])))
    if(write.individual.file){
      data.table::fwrite(parvo_settings, new_path, append = FALSE, col.names = FALSE, ...)
      data.table::fwrite(parvo_data[[2]], new_path, append = TRUE, ...)  
    }
    main.data <- plyr::rbind.fill(main.data, cbind(id, vo2max))
  }
  
  if(write.summary.file) data.table::fwrite(main.data, paste0(d, "/ouesSummary.csv"), ...)
  if(return.data) return(main.data)
} 


.parvo_extract_vo2_max_test_env <- function(data){
  # Test Environment
  location <- paste0(data[1, as.vector(!is.na(data[1, ]))==TRUE], " ", data[2, as.vector(!is.na(data[1, ]))==TRUE])
  starttime <- as.POSIXct(paste0(data[3, 2], "/", data[3, 4], "/", data[3, 6], " ", data[3, 7], ":", data[3,9], ":", data[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  date <- format(as.Date(starttime), "%B %d, %Y")
  InspTemp <- paste0(as.numeric(paste0(data[16,2]))*(9/5) + 32, " deg F")
  BaroPres <- paste(round(as.numeric(paste0(data[16,5])), 2), data[16, 6])
  ExpFlowTemp <- data[17, 2]
  BaseO2 <- round(as.numeric(data[18, 2]), 2)
  BaseCO2 <- round(as.numeric(data[18, 5]), 2)
  MeasuredO2 <- round(as.numeric(data[23, 8]), 2)
  MeasuredCO2 <- round(as.numeric(data[23, 11]), 2)
  
  rbind(
    paste("Location:", location), paste("Date:", date), paste("Start Time:", starttime), 
    paste("Insp Temp:", InspTemp), paste("Baro Press:", BaroPres), paste("Exp Flow Temp:", ExpFlowTemp),
    paste0("Base O2 (", BaseO2, ")", " and measured O2 (", MeasuredO2, ")"),
    paste0("Base CO2 (", BaseCO2, ")", " and measured CO2 (", MeasuredCO2, ")")
    )
}

.parvo_extract_vo2_max_participant_info <- function(data){
  name <- paste0(unlist(strsplit(stringr::str_replace(as.character(data[6,2]), " ", ""), "[,]"))[2], " ", unlist(strsplit(stringr::str_replace(as.character(data[6,2]), " ", ""), "[,]"))[1])
  age <- as.numeric(data[7, 2])
  gender <- ifelse(paste0(data[7, 5])=="F", "Female", "Male")
  height_in <- paste0(data[8,2])
  weight_lbs <- round((as.numeric(data[8,7])), 2)
  
  rbind(
    paste("Name: ", name), paste("Age:", age, "years"), paste("Gender:", gender),
    paste("Weight: ", weight_lbs, "lbs."), paste("Height:", height_in, "in.")
    )
}







.parvo_calculate_oues <- function(data){
  starttime <- as.POSIXct(paste0(data[3, 2], "/", data[3, 4], "/", data[3, 6], " ", data[3, 7], ":", data[3,9], ":", data[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  vo2 <- as.data.frame(data[31:nrow(data), 1:12])
  colnames(vo2) <- c("time.min", "vo2.l.min", "vo2.ml.kg.min", "mets", "rer", "hr.bpm", "ree", "tm.per.grade", "tm.speed", "ve.l.min", "vco2.l.min", 'resp.rate.bpm')
  vo2 <- vo2[!is.na(vo2$tm.speed), ]
  vo2 <- cbind(datetime = (starttime + (as.numeric(vo2$time.min)*60)), vo2)
  vo2 <- vo2 %>% dplyr::mutate_at(.vars = c(2:13), .funs = as.numeric)
  vo2$vo2.ml.min <- vo2$vo2.l.min * 1000
  vo2$log.ve.l.min <- log10(vo2$ve.l.min)
  
  # Extract VO2 max information
  eventSummary <- .parvo_extract_vo2_max_meta(data)[[1]]
  vo2Summary <- .parvo_extract_vo2_max_meta(data)[[2]]
  vo2MaxValue <- as.numeric(vo2Summary[vo2Summary$Measure=="vo2.ml.kg.min", 2])
  vo2$oues <- vo2$event <- NA
  
  # Identify events
  exerciseStart <- eventSummary[eventSummary$Event=="Start Exercise", 2]
  time2max100 <- vo2[which.max(vo2$vo2.ml.kg.min >= vo2MaxValue), "time.min"]
  time2max90 <- time2max100 * 0.9
  time2max75 <- time2max100 * 0.75
  
  # Flag events
  vo2[which.max(vo2$datetime >= exerciseStart), "event"] <- "start"
  vo2[which.max(vo2$vo2.ml.kg.min >= as.numeric(vo2MaxValue)), "event"] <- "vo2.max.100"
  vo2[which.max(vo2$time.min >= time2max90), "event"] <- "vo2.max.90"
  vo2[which.max(vo2$time.min >= time2max75), "event"] <- "vo2.max.75"
  
  # Get row indices
  start <- which(vo2$event=="start")
  vo2Max100 <- which(vo2$event == "vo2.max.100")
  vo2Max90 <- which(vo2$event == "vo2.max.90")
  vo2Max75 <- which(vo2$event == "vo2.max.75")
  
  # Calculate OUES
  fit100 <- stats::lm(vo2.ml.min ~ log.ve.l.min, data = vo2[start:vo2Max100, ])
  vo2[vo2Max100, "oues"] <- unname(fit100$coefficients[2])
  
  fit90 <- stats::lm(vo2.ml.min ~ log.ve.l.min, data = vo2[start:vo2Max90, ])
  vo2[vo2Max90, "oues"] <- unname(fit90$coefficients[2])
  
  fit75 <- stats::lm(vo2.ml.min ~ log.ve.l.min, data = vo2[start:vo2Max75, ])
  vo2[vo2Max75, "oues"] <- unname(fit75$coefficients[2])
  
  # Return OUES values and data
  list(oues = cbind(vo2[vo2Max75, "oues"], vo2[vo2Max90, "oues"], vo2[vo2Max100, "oues"]), data = vo2)
}


.parvo_extract_vo2_max_meta <- function(data){
  starttime <- as.POSIXct(paste0(data[3, 2], "/", data[3, 4], "/", data[3, 6], " ", data[3, 7], ":", data[3,9], ":", data[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  vo2 <- as.data.frame(data[31:nrow(data), 1:12])
  vo2 <- vo2[is.na(vo2$Col9), ]
  # Event Summary
  events <- grep("Events", vo2[, 1]) + 1
  eventSummary <- vo2[events:(events+3), 2:1]
  colnames(eventSummary) <- c("Event", "Time")
  rownames(eventSummary) <- NULL
  eventSummary$Time <- starttime + (as.numeric(eventSummary$Time)*60)
  # VO2 Max Summary
  maxValues <- vo2[which(vo2$Col1=="Max VO2"), 2:12][!is.na(vo2[which(vo2$Col1=="Max VO2"), 2:12])]
  measure <- c("vo2.l.min", "vo2.ml.kg.min", "mets")
  values <- c(as.numeric(maxValues[1]), as.numeric(maxValues[3]), as.numeric(maxValues[5]))
  vo2Summary <- data.frame(cbind(Measure = measure, Values = values))
  return(list(eventSummary, vo2Summary))
}












