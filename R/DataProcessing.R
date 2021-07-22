#' @title Mean and Standard Deviation
#' @description Prints out mean and standard deviation for a continuous variable.
#' @param x Name of a continuous variable in data.
#' @return Mean and Standard Deviation.
#' @details Prints out mean and standard deviation for a continuous variable.
#' @examples 
#' \dontrun{
#'  x = rnorm(100, 40, 5)
#'  meansd(x)
#'  }
#' @rdname meansd
#' @export 

meansd <- function(x) {
  paste0("Mean: ", round(mean(x, na.rm=TRUE), 2), "; ", "SD: ", round(sd(x, na.rm=TRUE), 2))
}

#' @title Frequency and Percentage
#' @description Prints out the frequency and percentage for a categorical or binary variable.
#' @param x Name of a categorical or binary variable in data.
#' @param total Number of total observations for the categorical or binary variable.
#' @return Frequency and percentage of the categorical or binary variable.
#' @details Prints out the frequency and percentage for a categorical or binary variable.
#' @examples 
#' \dontrun{
#' #EXAMPLE1
#' x = rnorm(100, 40, 5)
#' y = ifelse(x < 40, 1, 0)
#' freqper(y, 100)
#'  }
#' @rdname freqper
#' @export 

freqper <- function(x, total){
  x_table <- as.matrix(table(x))
  x_table <- cbind(x_table, round(((table(x)/total)*100), 2))
  cat(paste0(rownames(x_table), ": Frequency: ", x_table[, 1], "; ", "Percentage ", x_table[, 2], "%", "\n"))
}

#' @title Dummy 2 Categorical
#' @description Converts several related dummy variables to a single categorical variable.
#' @param data Data set where the variables can be located.
#' @param id The unique identifier for the participants in the data.
#' @param new_var The name of the new categorical variable that will be created.
#' @param column_list Combined list using c() of the data columns containing the dummy variables.
#' @param new_names The new categories that will be created within the categorical variable.
#' @return Returns the data set with the new categorical variable.
#' @details Converts several related dummy variables to a single categorical variable.
#' @examples 
#' \dontrun{
#'  #Add examples
#' }
#' @rdname dummy_2_categorical
#' @export 

dummy_2_categorical <- function(data, id, new_var, column_list, new_names){
  subset <- data[, c(id, column_list)]
  colnames(subset) <- c(id, new_names)
  subset[, new_var] <- names(subset[new_names])[max.col(subset[new_names])]
  subset[, new_names] <- NULL
  data <- merge(data, subset, by=id)
  return(data)
}

