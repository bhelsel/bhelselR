if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  ".", " Axis1", "HR", "counts", "Vector Magnitude", "age", "days",
  "diff.rq", "diff.ve.l.min", "diff.vo2.ml.kg.min", "interrupts",
  "month", "mvpa", "mvpa.bout.counts", "mvpa.bout.length", "pt", "season",
  "sedentary", "temp", "time.category", "time.group", "time.min",
  "timestamp", "valid_days", "var", "ve.l.min", "wear", "weekday",
  "vector.magnitude", "steps", "record.id"
))

#' @title bhelselR: A package for physical activity and weight management research.
#'
#' @description The bhelselR package provides several important functions:
#' 
#' @section bhelselR functions:
#' 
#' \code{\link{AG.temporal}}
#' 
#' \code{\link{agd_to_csv}}
#' 
#' \code{\link{AGread.csv}}
#' 
#' \code{\link{bhelselR}}
#' 
#' \code{\link{birth.date}}
#' 
#' \code{\link{child_bmi_percentile}}
#' 
#' \code{\link{cutpoints}}
#' 
#' \code{\link{dummy_2_categorical}}
#' 
#' \code{\link{freqper}}
#' 
#' \code{\link{gt3x2csv}}
#' 
#' \code{\link{mars.main}}
#' 
#' \code{\link{meansd}}
#' 
#' \code{\link{parvo.extract.data}}
#' 
#' \code{\link{parvo.extract.meta}}
#' 
#' \code{\link{parvo.ree.main}}
#' 
#' \code{\link{percent_over_bmi_50}}
#' 
#' \code{\link{read_agd}}
#' 
#' @docType package
#' @name bhelselR
NULL
