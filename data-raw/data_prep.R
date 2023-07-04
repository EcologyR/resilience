
exampledata <- read.csv("data-raw/DendroData.csv")

exampledata <- subset(exampledata, select = c("ID", "Year", "BAI"))
colnames(exampledata) <- c("ID" ,"temp", "performance")

exampledata$temp <- as.character(exampledata$temp)

usethis::use_data(exampledata, overwrite = TRUE)
