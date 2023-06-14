# https://www.rdocumentation.org/packages/dplR/versions/1.7.3/topics/read.tucson
library(dplR)

file_tuc <- read.tucson("R/Yellowstone_PSME_format.txt")

file_tuc$year <- row.names(file_tuc)

# pivot longer
file_tuc_l <- reshape(
  file_tuc,
  varying = names(file_tuc)[-which(names(file_tuc) == "year")],
  v.names = "ring_width",
  timevar = "tree_id",
  times = names(file_tuc)[-which(names(file_tuc) == "year")],
  direction = "long"
)

# arrange by tree_id
file_tuc_l <- file_tuc_l[order(file_tuc_l$tree_id), ]

# clean
rownames(file_tuc_l) <- NULL

file_tuc_l$id <- NULL

# test
tree_test <- subset(file_tuc_l, tree_id == "BTK05A")

identical(as.vector(tree_test[, "ring_width"]),
          as.vector(file_tuc[, "BTK05A"]))
