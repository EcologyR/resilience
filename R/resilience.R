#' Resilience indices
#'
#' @param series A [matrix] or [data.frame] representing a temporal series, with two columns: the dates (Character) and the performance (Numeric).
#' If `series` is a matrix, it must have only two columns: the first with dates and the second with performance data.
#' If `series` is a data.frame, it must contain at least two columns named `temp` and `performance`
#' with date and performance values, respectively. temp must be in a format "YYYY-MM-DD", "YYYY-MM", "YYYY".
#' Temp must be arranged from past to recent dates
#' @param event Character. List of dates or periods of the disturbance events.
#' @param lag Vector. Number of years, months or days to include in the calculations, pre and post disturbance
#' @param index Character. Indices to be calculated ('resilience', 'resistance' or 'recovery').
#' Various elements can be concatenated in the vector.
#'
#' @return A [data.frame]
#' @export
#'
#' @examples
# data(exampledata)
#' #
# tapply(exampledata, INDEX = exampledata$ID, resindex,
#        event = list("1990", "2000:2002"),
#        lag = c(4, 3),
#        index = c("resilience", "recovery", "resistance"))

resindex <- function(series, event, lag, index) {
  ## Check arguments

  # series
  if (!inherits(series, c("matrix", "data.frame"))) {
    stop("series must be either a matrix or a data.frame")
  }

  if (inherits(series, "matrix")) {
    stopifnot(ncol(series) == 3)
  }

  stopifnot("temp" %in% names(series))

  stopifnot("performance" %in% names(series))

  # event
  stopifnot(inherits(event, "list"))
  stopifnot(length(event) >= 1)

  # lag
  stopifnot(length(lag) == 2)

  # index
  if (any(!index %in% c("resilience", "resistance", "recovery"))) {
    stop("index must be one of 'resilience', 'resistance' or 'recovery'")
  }

  ##

  list_of_periods <- lapply(event, splitting, series1 = series, lag1 = lag)


  if ("resilience" %in% index) {
    out_resil <- data.frame(event = unlist(event))
    out_resil$resilience <- do.call(rbind, lapply(list_of_periods, resil))
  } else {
    out_resil <- NULL
  }

  if ("resistance" %in% index) {
    out_resis <- data.frame(event = unlist(event))
    out_resis$resistance <- do.call(rbind, lapply(list_of_periods, resist))
  } else {
    out_resis <- NULL
  }

  if ("recovery" %in% index) {
    out_recov <- data.frame(event = unlist(event))
    out_recov$recovery <- do.call(rbind, lapply(list_of_periods, recov))
  } else {
    out_recov <- NULL
  }

  out_l <- list(out_resil, out_resis, out_recov)
  out <- data.frame(event = unlist(event))

  for (i in 1:3) {
    if (is.null(out_l[[i]])) next
    out <- merge(out, out_l[[i]])
  }

  return(out)
}


# splitting temporal series

splitting <- function(event1, series1, lag1) {
  if (grepl("[0-9]{4}-[0-9]{2}", series1[, "temp"][1])) {
    lag1 <- months(lag1)
  } else {
    lag1 <- lag1
  }

  series1[, "temp"] <- temp_to_date(series1[, "temp"])

  if (!grepl(":", event1)) {
    event1 <- temp_to_date(event1)

    pre_period <- series1[series1[, "temp"] >= (event1 - lag1[1]) &
      series1[, "temp"] < event1, ]

    disturb_period <- series1[series1[, "temp"] == event1, ]

    pos_period <- series1[series1[, "temp"] > event1 &
      series1[, "temp"] <= (event1 + lag1[2]), ]
  } else {
    event2 <- strsplit(event1, split = ":")[[1]]

    event2 <- temp_to_date(event2)

    pre_period <- series1[series1[, "temp"] >= (event2[1] - lag1[1]) &
      series1[, "temp"] < event2[1], ]

    disturb_period <- series1[series1[, "temp"] >= event2[1] &
      series1[, "temp"] < event2[2], ]

    pos_period <- series1[series1[, "temp"] > event2[2] &
      series1[, "temp"] <= (event2[2] + lag1[2]), ]
  }

  return(list(
    "pre_period" = pre_period,
    "disturb_period" = disturb_period,
    "pos_period" = pos_period
  ))
}


# periods to Date format

temp_to_date <- function(period) {
  stopifnot(length(period) >= 1)

  stopifnot(is.character(period))

  if (grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", period[1])) {
    return(as.Date(period))
  } else {
    if (grepl("[0-9]{4}-[0-9]{2}", period[1])) {
      return(as.Date(paste0(period, "-01")))
    } else { # years
      return(as.numeric(period))
    }
  }
}


# ## check correct format ("YYYY-MM-DD")
# apply(ini.fin, c(1, 2), function(x) {
#
#   if (!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", x))
#
#     stop("Please provide dates as 'YYYY-MM-DD'")
#
# })


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
