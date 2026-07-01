if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".",
      "Var1",
      "Var2",
      "pt",
      "value",
      "var",
      "attachment",
      "data",
      "X1",
      "Presentations",
      "References",
      "extracted_dates",
      "extracted_years",
      "statistic",
      "p.value"
    )
  )
}

.studies <- tibble::tribble(
  ~abbreviation , ~grant_no     , ~studyTitle                                                                           , ~reporterTitle                                                                       , ~link                                                                             ,
  "SPARTANS"    , "K01AG083130" , "Supporting Physical Activity Research Techniques Across the Neurodivergent Spectrum" , "Assessment of Physical Activity for Alzheimer's Disease Research in Down Syndrome"  , "https://reporter.nih.gov/search/7bZI2MSiH0uEk-mkbbmPNg/project-details/11110354" ,
  "ABC-DS"      , "U19AG068054" , "Alzheimer's Biomarker Consortium - Down Syndrome"                                    , "Alzheimer Biomarker Consortium - Down Syndrome (ABC-DS)"                            , "https://reporter.nih.gov/search/OGAysePgWkmBA_jv5MlWXg/project-details/10906278" ,
  "TRC-DS"      , "R33AG066543" , "Trial Ready Cohort - Down Syndrome"                                                  , "Clinical trials to prevent Alzheimer's Disease in Down Syndrome"                    , "https://reporter.nih.gov/search/wJL7vSBVi0Gh64Eo4W2IoA/project-details/10913313" ,
  "BOLD"        , "R33AG078967" , "Brain Outcomes with Lifestyle Change in Down Syndrome"                               , "The Impact of Weight Loss on Alzheimer's Disease Risk in Adults with Down Syndrome" , "https://reporter.nih.gov/search/kCkPz-eDnUqZCOADQO8qtw/project-details/11173869" ,
  "KUADRC"      , "P30AG072973" , "University of Kansas Alzheimer's Disease Research Center (KU ADRC)"                  , "University of Kansas Alzheimer's Disease Research Center (KU ADRC)"                 , "https://reporter.nih.gov/search/wTNGmqICjEy2xgH2A53ZpA/project-details/11123101"
)
