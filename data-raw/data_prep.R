# https://www.rdocumentation.org/packages/dplR/versions/1.7.3/topics/read.tucson
library(dplR)

exampledata <- read.rwl("data-raw/ASE-R.rwl")

# exampledata <- subset(exampledata, select = c("ID", "Year", "BAI"))
# colnames(exampledata) <- c("ID" ,"temp", "performance")
#
# exampledata$temp <- as.character(exampledata$temp)

usethis::use_data(exampledata, overwrite = TRUE)
