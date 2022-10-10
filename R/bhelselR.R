if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", " Axis1", "HR", "counts", "Vector Magnitude", "age", "days",
  "diff.rq", "diff.ve.l.min", "diff.vo2.ml.kg.min", "interrupts",
  "month", "mvpa", "mvpa.bout.counts", "mvpa.bout.length", "pt", "season",
  "sedentary", "temp", "time.category", "time.group", "time.min",
  "timestamp", "valid_days", "var", "vo2.l.min", "ve.l.min", "wear", "weekday",
  "vector.magnitude", "steps", "record.id", "Var1", "Var2", "value",
  "time", "Date", " Time", "Axis1", "Vector.Magnitude", " Time"))

#' @title bhelselR: A package for physical activity and weight management research.
#'
#' @description The bhelselR package provides several important functions:
#' 
#' @section Data Processing:
#' 
#' \code{\link{child_bmi_percentile}}
#' 
#' \code{\link{percent_over_bmi_50}}
#' 
#' \code{\link{dummy_2_categorical}}
#' 
#' \code{\link{freqper}}
#' 
#' \code{\link{meansd}}
#' 
#' \code{\link{ilr_transform}}
#' 
#' @section Accelerometer:
#' 
#' \code{\link{mars.main}}
#' 
#' \code{\link{read_agd}}
#' 
#' \code{\link{agd_to_csv}}
#' 
#' \code{\link{gt3x2csv}}
#' 
#' \code{\link{AGread.csv}}
#' 
#' \code{\link{birth.date}}
#' 
#' \code{\link{AG.temporal}}
#' 
#' \code{\link{cutpoints}}
#' 
#' \code{\link{detect.bouts}}
#' 
#' @section Energy Expenditure:
#' 
#' \code{\link{parvo.extract.data}}
#' 
#' \code{\link{parvo.extract.meta}}
#' 
#' \code{\link{parvo.ree.main}}
#' 
#' \code{\link{parvo.aee.final4}}
#' 
#' @section Visualization:
#' 
#' \code{\link{cor_matrix}}
#' 
#' @section Utilities:
#' 
#' \code{\link{sort_files_in_downloads}}
#' 
#' @docType package
#' @name bhelselR
#' @import dplyr
#' @import magrittr

NULL
