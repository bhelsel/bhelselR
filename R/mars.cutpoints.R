#' @title cut.points
#' @description This function applies commonly used cutpoints to accelerometer data and returns the intensity.
#' @param data Accelerometer data set with a time stamp and either counts or vector magnitude.
#' @param set.name Cutpoint set to be used. Options currently include freedson.child, troiano.adult, fariasbarnett.adult, or freedson.adult.
#' @param n.axis Number of axes to be used. Options currently include 1 (vertical axis) or 3 (vector magnitude)
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
#' @rdname cut.points
#' @export 
#' @importFrom DescTools Mode
#' @importFrom stringr str_split
#' @importFrom PhysicalActivity wearingMarking

cut.points <- function(data, sets, set.name, n.axis) {
  
  cols <- colnames(data)
  
  try(cur_set <- sets[[set.name]][[n.axis]], 
      stop("Error: cut-point set not found. Make sure the set name and/or number of axes are correct"))
  
  if(DescTools::Mode(data$age)[1]<18 & set.name=="freedson.child"){
    cur_set <- unlist(cur_set[as.character(DescTools::Mode(data$age)[1])], use.names=FALSE)}
  
  intensity <- sapply(cur_set[1:length(cur_set)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[1])
  
  values <- sapply(cur_set[1:length(cur_set)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[2])
  
  cutpoint_matrix <- as.matrix(cbind(intensity, values))
  
  data <- PhysicalActivity::wearingMarking(dataset = data, frame = 60, perMinuteCts = 1, TS = "time.stamp", cts = "counts",
                                           allowanceFrame = 2, newcolname = "wear", tz = "America/Chicago")
  
  # Vertical Axis
  if(n.axis=="1"){data$intensity <- cut(data$counts, 
                                   breaks = c(0, as.numeric(cutpoint_matrix[, 2]), max(data$counts)), 
                                   labels = c("sedentary", cutpoint_matrix[, 1]), right = FALSE, include.lowest = TRUE)}
  
  # Vector Magnitude
  if(n.axis=="3"){data$intensity <- cut(data$vector.magnitude, 
                                   breaks = c(0, as.numeric(cutpoint_matrix[, 2]), max(data$vector.magnitude)), 
                                   labels = c("sedentary", cutpoint_matrix[, 1]), right = FALSE, include.lowest = TRUE)}
  
  if("sedentary" %in% unique(data$intensity)){data$sedentary <- ifelse(data$intensity=="sedentary" & data$wear=="w", 1, 0)}
  
  if("light" %in% unique(data$intensity)){data$light <- ifelse(data$intensity=="light" & data$wear=="w", 1, 0)}
  
  if("moderate" %in% unique(data$intensity)){data$moderate <- ifelse(data$intensity=="moderate" & data$wear=="w", 1, 0)}
  
  if("vigorous" %in% unique(data$intensity)){data$vigorous <- ifelse(data$intensity=="vigorous" & data$wear=="w", 1, 0)}
  
  if("very.vigorous" %in% unique(data$intensity)){data$very.vigorous <- ifelse(data$intensity=="very.vigorous" & data$wear=="w", 1, 0)}
  
  if("mvpa" %in% unique(data$intensity)){data$mvpa <- ifelse(data$intensity=="mvpa" & data$wear=="w", 1, 0)}
  
  if("mvpa" %in% unique(data$intensity)==FALSE){
    data$mvpa <- ifelse(data$intensity %in% c("moderate", "vigorous", "very.vigorous") & data$wear=="w", 1, 0)
  }
  
  data$wear <- ifelse(data$wear=="w", 1, 0)
  
  data <- data[, c(cols[1:3], "days", cols[4:length(cols)], "wear", names(dplyr::select(data, sedentary:mvpa)))]
  
  return(data)
}














