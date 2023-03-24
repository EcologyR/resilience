
exampledata <- read.csv("data-raw/DendroData.csv")
exampledata <- exampledata |>
  select(ID, Year, BAI) |>
  rename(temp = Year, performance = BAI)

usethis::use_data(exampledata, overwrite = TRUE)
