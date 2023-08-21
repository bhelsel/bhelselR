# Copyright © 2021 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "Var1", "Var2", "pt", "value", "var", "attachment"))

#' @title bhelselR: A package for physical activity and weight management research.
#'
#' @description The bhelselR package provides several important functions:
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
#' @section Visualization:
#' 
#' \code{\link{cor_matrix}}
#' 
#' @section Utilities:
#' 
#' \code{\link{sort_files_in_downloads}}
#' 
#' \code{\link{dpawmReliabilityTesting}}
#' 
#' @docType package
#' @name bhelselR
#' @import dplyr
#' @import magrittr

NULL
