library(magrittr)
library(readr)

japan_eq_3_11 = 'extdata/earthquake_data/query.csv' %>%
  system.file(package = "readtv", mustWork = TRUE) %>%
  read_csv
usethis::use_data(japan_eq_3_11, overwrite = TRUE)
