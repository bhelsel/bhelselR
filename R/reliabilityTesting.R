#' @title dpawmReliabilityTesting
#' @description Calculates ICC values for DPAWM Reliability Testing and creates a Boxplot of the measures
#' @param path Full path name to the CSV file containing the reliability measures to be analyzed
#' @param na.rm logical, indicating whether NA values should be stripped before the computation proceeds. If set to TRUE only the complete cases of the ratings will be used. Defaults to FALSE.
#' @return Prints out ICC statistics and returns a plot to R
#' @details Calculates ICC values for DPAWM Reliability Testing and creates a Boxplot of the measures
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  dpawmReliabilityTesting(path = "full/path/to/file.csv", na.rm = TRUE)
#'  }
#' }
#' @seealso 
#'  \code{\link[DescTools]{ICC}}
#'  \code{\link[dplyr]{mutate_all}}, \code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{geom_jitter}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{scale_x_discrete}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{margin}}
#' @rdname dpawmReliabilityTesting
#' @export 
#' @importFrom DescTools ICC
#' @importFrom dplyr mutate_if filter
#' @importFrom tidyr pivot_longer
#' @import ggplot2

dpawmReliabilityTesting <- function(path, na.rm = FALSE){
  data <- read.csv(path)
  # ICC3 (removes missing values)
  myicc <- DescTools::ICC(as.matrix(data[, -1]), conf.level = 0.95, na.rm = na.rm)$results %>% dplyr::mutate_if(is.numeric, round, 2)
  msg <- sprintf("ICC for a Fixed Set of Raters %g (95 CI: %g - %g).", myicc$est[3], myicc$lwr.ci[3], myicc$upr.ci[3])
  
  cat(
    " Less than 0.50: Poor reliability", "\n",
    "Between 0.5 and 0.75: Moderate reliability", "\n",
    "Between 0.75 and 0.9: Good reliability", "\n",
    "Greater than 0.9: Excellent reliability", "\n"
  )
  
  print(myicc)
  
  mycolors <- c("#a40000", "#16317d", "#007e2f", "#ffcd12", "#b86092", "#721b3e", "#00b7a7")
  
  plotData <- data %>% tidyr::pivot_longer(cols = 2:ncol(.)) %>% dplyr::filter(!is.na(value))
  no.subjects <- length(unique(plotData$subject))
  no.raters <- length(unique(plotData$name))
  minValue <- floor(min(plotData$value, na.rm = TRUE))
  maxValue <- ceiling(max(plotData$value, na.rm = TRUE))
  name <- subject <- NULL
  
  ggplot2::ggplot(data = plotData, ggplot2::aes(x = subject, y = value)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(ggplot2::aes(color = name), size = 2) +
    ggplot2::scale_color_manual(values = mycolors[1:no.raters]) +
    ggplot2::scale_x_discrete(labels = seq(1, no.subjects, 1)) +
    ggplot2::scale_y_continuous(breaks = seq(minValue, maxValue, 2)) +
    ggplot2::labs(x = "Subject", y = "Measurement",
         title = "Reliability Testing for the Division of Physical Activity and Weight Management",
         subtitle = msg,
         caption = "Shrout, P. E., Fleiss, J. L. (1979) Intraclass correlations: uses in assessing rater reliability. Psychological Bulletin, 86, 420-3428.") +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 14),
          axis.line = ggplot2::element_line(linewidth = 1, color = "black"),
          legend.title = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 12),
          plot.caption = ggplot2::element_text(size = 12, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
          axis.title.x = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(t = 25, b = 15)),
          axis.title.y = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(r = 25)),
          plot.title = ggplot2::element_text(size = 16, face = "bold", margin = ggplot2::margin(t = 15, b = 15), hjust = 0.5),
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid = ggplot2::element_line(color = "grey92"))
}
