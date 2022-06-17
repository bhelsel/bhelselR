#' @title cutpoints
#' @description This function applies commonly used cutpoints to accelerometer data and returns the intensity.
#' @param data Accelerometer data set with a time stamp and either counts or vector magnitude.
#' @param sets Set of accelerometer cutpoints in a list format
#' @param set.name Cutpoint set to be used. Options currently include freedson.child, troiano.adult, fariasbarnett.adult, or freedson.adult.
#' @param n.axis Number of axes to be used. Options currently include 1 (vertical axis) or 3 (vector magnitude)
#' @param spurious Maximum acceptable counts before the device is considered to be malfunctioning, Default: 20000
#' @return Returns accelerometer data set with an intensity value that also include nonwear time.
#' @details This function applies commonly used cutpoints to accelerometer data and returns the intensity.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[DescTools]{Mode}}
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[PhysicalActivity]{wearingMarking}}
#' @rdname cutpoints
#' @export 
#' @importFrom DescTools Mode
#' @importFrom stringr str_split
#' @importFrom PhysicalActivity wearingMarking

cutpoints <- function(data, sets, set.name, n.axis, spurious = 20000) {
  
  cols <- colnames(data)
  
  try(cur_set <- sets[[set.name]][[n.axis]], 
      stop("Error: cut-point set not found. Make sure the set name and/or number of axes are correct"))
  
  if(set.name=="freedson.child" & exists("age", data)){
    if(DescTools::Mode(data$age)[1]<18){
      cur_set <- unlist(cur_set[as.character(DescTools::Mode(data$age)[1])], use.names=FALSE)
    } else{
        stop("Age is > 18 years. The Freedson age-specific cut-points cannot be applied.")
      }
    }
  
  intensity <- sapply(cur_set[1:length(cur_set)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[1])
  
  values <- sapply(cur_set[1:length(cur_set)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[2])
  
  cutpoint_matrix <- as.matrix(cbind(intensity, values))
  
  data <- PhysicalActivity::wearingMarking(dataset = data, frame = 90, perMinuteCts = 1, TS = "time.stamp", cts = "counts",
                                           allowanceFrame = 2, newcolname = "wear", tz = "America/Chicago")
  
  # Vertical Axis
  if(n.axis=="1"){data$intensity <- cut(data$counts, 
                                   breaks = c(0, as.numeric(cutpoint_matrix[, 2]), spurious-1), 
                                   labels = c("sedentary", cutpoint_matrix[, 1]), right = FALSE, include.lowest = TRUE)}
  
  # Vector Magnitude
  if(n.axis=="3"){data$intensity <- cut(data$vector.magnitude, 
                                   breaks = c(0, as.numeric(cutpoint_matrix[, 2]), spurious-1), 
                                   labels = c("sedentary", cutpoint_matrix[, 1]), right = FALSE, include.lowest = TRUE)}
  
  data$sedentary <- ifelse(data$intensity=="sedentary" & data$wear=="w", 1, 0)
  
  if("light" %in% cutpoint_matrix[, 1]){data$light <- ifelse(data$intensity=="light" & data$wear=="w", 1, 0)}
  
  if("moderate" %in% cutpoint_matrix[, 1]){data$moderate <- ifelse(data$intensity=="moderate" & data$wear=="w", 1, 0)}
  
  if("vigorous" %in% cutpoint_matrix[, 1]){data$vigorous <- ifelse(data$intensity=="vigorous" & data$wear=="w", 1, 0)}
  
  if("very.vigorous" %in% cutpoint_matrix[, 1]){data$very.vigorous <- ifelse(data$intensity=="very.vigorous" & data$wear=="w", 1, 0)}
  
  if("mvpa" %in% cutpoint_matrix[, 1]){data$mvpa <- ifelse(data$intensity=="mvpa" & data$wear=="w", 1, 0)}
  
  if("mvpa" %in% cutpoint_matrix[, 1]==FALSE){
    data$mvpa <- ifelse(data$intensity %in% c("moderate", "vigorous", "very.vigorous") & data$wear=="w", 1, 0)
  }
  
  data <- detect.bouts(data, bout=10, tolerance = 2)
  
  data$wear <- ifelse(data$wear=="w", 1, 0)
  
  data <- data[, c(cols[1:3], "days", cols[4:length(cols)], "wear", names(dplyr::select(data, sedentary:mvpa.bout.counts)))]
  
  return(data)
  
}

#' @title Detect bouts
#' @description Detect bouts of moderate-to-vigorous physical activity
#' @param data Data that has MVPA has a binary variable with 1 indicating MVPA and a days variable (integer) to indicate study day
#' @param bout The threshold to consider as a bout of physical activity, Default: 10
#' @param tolerance Minute allowance greater than the tolerance that is required to terminate a bout of physical activity, Default: 2
#' @return Dataset with the added bout length and bout count variables
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname detect.bouts
#' @export
#' @importFrom tidyr drop_na

detect.bouts <- function(data, bout=10, tolerance=2){
  
  `%>%` <- dplyr::`%>%`
  
  data$interrupts <- ifelse(data$mvpa==0, 1, 0)
  
  data <- data %>% dplyr::group_by(days, temp=cumsum(interrupts==0)) %>% dplyr::mutate(interrupts = cumsum(interrupts)) %>% dplyr::ungroup() %>% dplyr::select(-temp)
  
  data$mvpa.bout.length <- ifelse(dplyr::lead(data$interrupts, n=tolerance+1)==tolerance+1, 1, 0) # Identify the last minute of the bout
  
  data$interrupts <- ifelse(dplyr::lag(data$mvpa.bout.length, n=tolerance)==1, 0, data$interrupts) # Remove interrupt indicator at tolerance level if the bout ends
  
  data$interrupts <- ifelse(data$interrupts > tolerance, 0, data$interrupts) # Remove all interrupt indicators that are greater than tolerance
  
  data$interrupts <- ifelse(dplyr::lead(data$interrupts, n=1)!=tolerance & data$interrupts!=tolerance & dplyr::lag(data$mvpa.bout.length, n=1)==1, 0, data$interrupts)
  
  data$interrupts <- ifelse(data$interrupts!=0, 1, 0) # Change interrupt indicator at tolerance that did not end the bout to 1 so it can be counted as part of the bout
  
  data$mvpa.bout.length <- data$mvpa + data$interrupts # Add MVPA and interrupts together to mark the entire bout
  
  data <- data %>% tidyr::drop_na(interrupts, mvpa.bout.length) %>% dplyr::group_by(days, temp=cumsum(mvpa.bout.length==0)) %>% dplyr::mutate(mvpa.bout.length = cumsum(mvpa)) %>% dplyr::ungroup() %>% dplyr::select(-temp)
  
  data$mvpa.bout.length <- ifelse(dplyr::lead(data$mvpa.bout.length, n=1)==0 & data$mvpa.bout.length!=0, data$mvpa.bout.length, 0) # Keep only the highest number of the bout
  
  data$mvpa.bout.length <- ifelse(data$mvpa.bout.length >= bout, data$mvpa.bout.length, 0) # Remove all bouts that are below the threshold
  
  data$mvpa.bout.counts <- ifelse(data$mvpa.bout.length!=0, 1, 0) # Add a bout indicator that can be added to get total bout count
  
  data$interrupts <- NULL
  
  return(data)
  
}