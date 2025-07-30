#' @title brainpower_merge_files
#' @description Merges the data files from the Brain Power study uploaded to the INCLUDE DCC
#' @param datadir Data directory where the data files exisit
#' @return The merged data set
#' @details Merges the data files from the Brain Power study uploaded to the INCLUDE DCC
#' @rdname brainpower_merge_files
#' @export 

brainpower_merge_files <- function(datadir){
  files <- list.files(datadir, full.names = TRUE)
  data <- data.frame()
  filtered_files <- files[!grepl("Demo|Activity Day|Random", files)]
  
  for(file in filtered_files){
    if(nrow(data) == 0){
      data <- read.csv(file)
      } else{
      data <- merge(
        x = data, 
        y = read.csv(file), 
        by = c("id", "timepoint"), 
        all = TRUE
        )
      }
    }

  # Add Demographics and Randomization
  demo <- read.csv(files[grepl("Demographics", files)])
  rand <- read.csv(files[grepl("Randomization", files)])
  data <- merge(
    x = merge(
      x = demo[, -which(colnames(demo) == "timepoint")], 
      y = rand, 
      by = "id"), 
    y = data, 
    by = "id")
  
  data <- data[data$timepoint %in% c(1,3,5), ]
  order <- c("id", "timepoint", 
             names(data)[-c(which(names(data) == "id"), which(names(data) == "timepoint"))])
  data <- data[, order]
  return(data)
}

#' @title brainpower_add_labels
#' @description Adds labels and factor labels from the Brain Power data dictionary. 
#' @param data The data set containing the columns and factors to label
#' @param dictionary A path to the data dictionary or the data dictionary loaded into R as a dataframe
#' @param add_factor_labels Adds labels to factor variables as represented in the data dictionary, Default: TRUE
#' @return A labeled data set
#' @details Adds labels and factor labels from the Brain Power data dictionary. The
#' data dictionary can be accessed after installing the bhelselR package with 
#' `system.file("extdata/BrainPowerDataDictionary.csv", package = "bhelselR")`
#' @rdname brainpower_add_labels
#' @export 


brainpower_add_labels <- function(data, dictionary, add_factor_labels = TRUE){
  if(is.character(data)) data <- read.csv(data)
  if(is.character(dictionary)) dictionary <- read.csv(dictionary)
  if(add_factor_labels){
    for(i in names(data)){
      if(i %in% dictionary$variable){
        desc <- dictionary$description[which.max(dictionary$variable == i)]
        if(grepl(":", desc)){
            desc <- 
              gregexpr("\\((.*?)\\)", desc) |>
              regmatches(x = desc) |>
              (\(x) x[[1]])() |>
              gsub(pattern = "[()]", replacement = "") |>
              strsplit("; ") |>
              unlist() |>
              (\(x) x[grepl(": ", x)])()
          levels <- sapply(desc, function(x) as.numeric(sub(":.*", "", x)))
          labels <- sapply(desc, function(x) sub(".*: ", "", x))
          data[, i] <- factor(data[, i], levels = levels, labels = labels)
          }
        }
      }
    }
  # Apply Labels
  for(i in names(data)){
    attr(data[[i]], "label") <- 
      dictionary$label[which.max(dictionary$variable == i)]
  }
  return(data)
}