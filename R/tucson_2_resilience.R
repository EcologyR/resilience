load("data/exampledata.rda")

exampledata$year <- row.names(exampledata)

# long format
exampledata_l <- reshape(
  exampledata,
  varying = names(exampledata)[-which(names(exampledata) == "year")],
  v.names = "ring_width",
  timevar = "tree_id",
  times = names(exampledata)[-which(names(exampledata) == "year")],
  direction = "long"
)

# arrange by tree_id
exampledata_l <- exampledata_l[order(exampledata_l$tree_id), ]

# clean
rownames(exampledata_l) <- NULL

exampledata_l$id <- NULL

# test
tree_test <- subset(exampledata_l, tree_id == "A-R-10a")

identical(as.vector(tree_test[, "ring_width"]),
          as.vector(exampledata[, "A-R-10a"]))
