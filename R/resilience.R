
#' Resilience indices
#'
#' @param series A [matrix] or [data.frame] representing a temporal series, with two columns: the dates (Date) and the performance (Numeric).
#' If `series` is a matrix, it must have only two columns: the first with dates and the second with performance data.
#' If `series` is a data.frame, it must contain at least two columns named `temp` and `performance`
#' with date and performance values, respectively.
#' @param event Date. Vector of dates of the disturbance events
#' @param lag Vector. Number of temporal steps to include in the calculations, pre and post disturbance
#' @param index Character. Indices to be calculated ('resilience', 'resistance' or 'recovery').
#' Various elements can be concatenated in the vector.
#'
#' @return A [data.frame]
#' @export
#'
#' @examples
# data(exampledata)
# for (i in 1:length(unique(exampledata$ID))) {
# item <- exampledata[exampledata$ID == unique(exampledata$ID)[i], ]
# print(resindex(series = item,
#          event = c(1990, 2000),
#          lag = c(3, 3),
#         index = c("resilience", "recovery", "resistance")))
# }

resindex <- function(series, event, lag, index) {
  ## Check arguments

  # series
  if (!inherits(series, c("matrix", "data.frame"))) {
    stop("series must be either a matrix or a data.frame")
  }

  if (!inherits(series, "matrix")) {
    stopifnot(ncol(series) == 3)
  }

  stopifnot("temp" %in% names(series))

  stopifnot("performance" %in% names(series))

  # event
  stopifnot(event >= 1)
  stopifnot(class(event[1]) != class(series$temp[1]))

  # lag
  stopifnot(length(lag) == 2)

  # index
  if (any(!index %in% c("resilience", "resistance", "recovery"))) {
    stop("index must be one of 'resilience', 'resistance' or 'recovery'")
  }

  ##

  list_of_periods <- lapply(event, splitting, series1 = series, lag1 = lag)


  if ("resilience" %in% index) {
    out_resil <- data.frame(event = event)
    out_resil$resilience <- do.call(rbind, lapply(list_of_periods, resil))
  } else {
    out_resil <- NULL
  }

  if ("resistance" %in% index) {
    out_resis <- data.frame(event = event)
    out_resis$resistance <- do.call(rbind, lapply(list_of_periods, resist))

  } else {
    out_resis <- NULL
  }

  if ("recovery" %in% index) {
    out_recov <- data.frame(event = event)
    out_recov$recovery <- do.call(rbind, lapply(list_of_periods, recov))
  } else {
    out_recov <- NULL
  }

  out_l <- list(out_resil, out_resis, out_recov)
  out <- data.frame(event = event)

  for (i in 1:3) {
    if (is.null(out_l[[i]])) next
    out <- merge(out, out_l[[i]])
  }

  return(out)
}


# splitting temporal series

splitting <- function (event1, series1, lag1) {

  pre_period <- series1[series1$temp >= (event1 - lag1[1]) &
                          series1$temp < event1, ]

  disturb_period <- series1[series1$temp == event1, ]

  pos_period <- series1[series1$temp > event1 &
                           series1$temp <= (event1 + lag1[2]), ]

  return(list("pre_period" = pre_period,
              "disturb_period" = disturb_period,
              "pos_period" = pos_period))

}


# resilience
resil <- function(x) {
  return(mean(x$pos_period[, "performance"]) / mean(x$pre_period[, "performance"]))
}

# resistance
resist <- function(x) {
  return(mean(x$disturb_period[, "performance"]) / mean(x$pre_period[, "performance"]))
}

# recovery
recov <- function(x) {
  return(mean(x$pos_period[, "performance"]) / mean(x$disturb_period[, "performance"]))
}
