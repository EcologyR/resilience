#' From Tucson to long format
#'
#' @param series a [data.frame] containing tree-ring ring widths in Tucson format with the series in columns and the years as rows. The series ids are the column names and the years are the row names.
#'
#' @return a [data.frame] in long format with three columns named `ID`, `temp` and `performance`, with ID, date, and performance values, respectively.
#' @export
#'
#' @examples
#' data(exampledata)
#' tucson_2_resilience(series = exampledata)

tucson_2_resilience <- function(series) {

  # check arguments
  if (!inherits(series, "data.frame")) {
    stop("series must be a data.frame")
  }

  series$temp <- row.names(series)

  # long format
  series_l <- reshape(
    series,
    varying = names(series)[-which(names(series) == "temp")],
    v.names = "performance",
    timevar = "ID",
    times = names(series)[-which(names(series) == "temp")],
    direction = "long"
  )

  # arrange by ID
  series_l <- series_l[order(series_l$ID), ]

  # clean
  rownames(series_l) <- NULL

  series_l$id <- NULL

  col_order <- c("ID" ,"temp", "performance")

  series_l_o <- series_l[, col_order]

  series_l_o$temp <- as.character(series_l_o$temp)

  return(series_l)

}

# kk <- tucson_2_resilience(series = exampledata)
# resindex(
#   series = kk,
#   event = list("1990", "2000:2002"),
#   lag = c(4, 3),
#   index =  c("resilience", "recovery", "resistance")
# )

