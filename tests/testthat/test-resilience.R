test_that("resilience index is working", {
  data(exampledata)
  df <- exampledata[exampledata$ID == "AP39", ]
  expect_equal(resindex(df, event = 2005, ntemp = 2, index = "resilience"),
               sum(df[df$temp == 2003:2004,"performance"])/sum(df[df$temp == 2006:2007,"performance"]))
})
