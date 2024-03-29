% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BMIpercentiles.R
\name{percent_over_bmi_50}
\alias{percent_over_bmi_50}
\title{percent_over_bmi_50}
\usage{
percent_over_bmi_50(id, gender, age, bmi)
}
\arguments{
\item{id}{A unique study participant identifier}

\item{gender}{Gender as a character variable, i.e., Male, Female}

\item{age}{Age in years as a numeric variable}

\item{bmi}{Body mass index in kg/m^2 as a numeric variable}
}
\value{
Dataframe with id and body mass index percentile rounded to 4 digits.
}
\description{
Calculate percent over the 50th percentile child body mass index.
}
\details{
This function can be used to calculate child body mass index percentiles for those ≤ 20 years.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
