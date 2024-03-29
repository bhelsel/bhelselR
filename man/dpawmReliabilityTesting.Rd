% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliabilityTesting.R
\name{dpawmReliabilityTesting}
\alias{dpawmReliabilityTesting}
\title{dpawmReliabilityTesting}
\usage{
dpawmReliabilityTesting(path, na.rm = FALSE)
}
\arguments{
\item{path}{Full path name to the CSV file containing the reliability measures to be analyzed}

\item{na.rm}{logical, indicating whether NA values should be stripped before the computation proceeds. If set to TRUE only the complete cases of the ratings will be used. Defaults to FALSE.}
}
\value{
Prints out ICC statistics and returns a plot to R
}
\description{
Calculates ICC values for DPAWM Reliability Testing and creates a Boxplot of the measures
}
\details{
Calculates ICC values for DPAWM Reliability Testing and creates a Boxplot of the measures
}
\examples{
\dontrun{
if(interactive()){
 dpawmReliabilityTesting(path = "full/path/to/file.csv", na.rm = TRUE)
 }
}
}
\seealso{
\code{\link[DescTools]{ICC}}
\code{\link[dplyr]{mutate_all}}, \code{\link[dplyr]{filter}}
\code{\link[tidyr]{pivot_longer}}
\code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{geom_jitter}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{scale_x_discrete}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{margin}}
}
