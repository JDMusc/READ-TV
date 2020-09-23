library(magrittr)
library(readr)

loadCovid = function(pth)
  paste0('extdata/covid_data/', pth) %>%
  system.file(package = "readtv", mustWork = TRUE) %>%
  read_csv

covid_global = loadCovid('global/owid-covid-data.csv')
usethis::use_data(covid_global, overwrite = TRUE)

covid_usa = loadCovid('states/us-states.csv')
usethis::use_data(covid_usa, overwrite = TRUE)
