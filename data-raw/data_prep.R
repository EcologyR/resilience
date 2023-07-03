
exampledata <- read.csv("data-raw/DendroData.csv")

exampledata <- subset(exampledata, select = c("ID", "Year", "BAI"))
colnames(exampledata) <- c("ID" ,"temp", "performance")

usethis::use_data(exampledata, overwrite = TRUE)
