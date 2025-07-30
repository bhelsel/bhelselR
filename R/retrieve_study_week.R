#' @title retrieve_study_week
#' @description Retrieves the study week for each person in a data set with sequential dates
#' @param data The data set with at least a date and id column
#' @param date The name of the column storing the sequential dates
#' @param first_day Day of the week that the study starts, Default: 'Monday'
#' @param id The name of the column storing the participant IDs
#' @return A tibble with the study weeks added to the data set
#' @details Retrieves the study week for each person in a data set with sequential dates. Zeros are
#' added if the dates start before first_day.
#' @seealso 
#'  \code{\link[dplyr]{group_by_all}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[lubridate]{day}}
#' @rdname retrieve_study_week
#' @export 
#' @importFrom dplyr group_by_at summarize
#' @importFrom lubridate wday


retrieve_study_week <- function(data, date, first_day = "Monday", id) {
  
  # Get the day of the week for each date
  data$WeekDay <- weekdays(as.Date(data[[date]]))

  # Get the first first_day for each person
  MinD <- data %>%
    dplyr::group_by_at(id) %>%
    dplyr::summarize(MinDate = as.Date(min(.data[[date]])))

  # with the first day merge to the data
  data <- merge(data, MinD, by = id, all.x = T, all.y = T)

  # Get the day number of the study
  data$StudyDay <- as.integer(data[[date]] - data$MinDate) + 1
  # Get the first first_day Date
  Day1data <- data[data$StudyDay == 1, ]
  # First WeekDay
  # Get numeric day of week (1 = Sunday, 7 = Saturday)
  Day1data$WeekDayNum <- lubridate::wday(Day1data$MinDate)
  # Vector of days of the week
  DoW <- tolower(c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  ))
  # Get the number of days to add to the Start Date
  Day1data$TargetDayNum <- match(tolower(first_day), DoW)

  # Calculate days to add
  Day1data$Days2Add <- (Day1data$TargetDayNum - Day1data$WeekDayNum + 7) %% 7

  # Get the first day of the study Week
  Day1data$StdyWkDay1 <- Day1data$MinDate + Day1data$Days2Add

  # Keep only the needed columns to merge
  data <- merge(
    data,
    Day1data[, c(id, 'StdyWkDay1')],
    by = id,
    all.x = T,
    all.y = T
  )

  # Get the Study Week Number
  data$StudyWeek <- floor((as.integer(data[[date]] - data$StdyWkDay1)) / 7) + 1

  # Remove columns that are not needed and used to make sure the function works
  data$MinDate <- data$StdyWkDay1 <- NULL
  class(data) <- c("tbl", "tbl_df", "data.frame")
  return(data)
}
