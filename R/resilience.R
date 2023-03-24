
#' Resilience indices
#'
#' @param data A [matrix] or [data.frame] with two columns temp (Date) and performance (Numeric)
#' @param event Date. Vector of dates of the disturbance events
#' @param ntemp Double. Number of temporal steps to include in the calculations
#' @param index Character. Indices to be calculated ('resilience', 'resistance' or 'recovery').
#' Various elements can be concatenated in the vector.
#'
#' @return A [data.frame]
#' @export
#'
#' @examples

resindex <- function(data, event, ntemp, index) {

  ##Check arguments

  #data
  if (!inherits(data, c("matrix", "data.frame")))
    stop("data must be either a matrix or a data.frame")

  if (!inherits(data, "matrix")) {
    stopifnot(ncol(data) == 2)
  }

  stopifnot("temp" %in% names(data))

  stopifnot("performance" %in% names(data))

  #event
  stopifnot(class(event[1]) != class(data$temp[1]))

  #ntemp
  stopifnot(event >= 1)

  #index
  if (!index %in% c("resilience", "resistance", "recovery"))
    stop("index must be one of 'resilience', 'resistance' or 'recovery'")

  ##

  l <- vector("list", length = length(event))

  for (i in 1:length(event)) {
    l[[i]] <- data[data$temp > (event[i] - ntemp - 1) &
                     data$temp < (event[i] + ntemp + 1), ]
  }


  if ("resilience" %in% index) {
    out_resil <- data.frame(resilience = do.call(rbind, lapply(l, resil)))
    out_resil$event <- event
  } else { out_resil <- NULL }

  if ("resistance" %in% index) {
      out_resis <- data.frame(resistance = do.call(rbind, lapply(l, resist)))
      out_resis$event <- event
  } else { out_resis <- NULL }

  if ("recovery" %in% index) {
      out_recov <- data.frame(recovery = do.call(rbind, lapply(l, recov)))
      out_recov$event <- event
  } else { out_recov <- NULL }

  out_l <- list(out_resil, out_resis, out_recov)
  out <- data.frame(event=event)

  for (i in 1:3) {
    if (is.null(out_l[[i]])) next
    out <- merge(out, out_l[[i]])
  }

  return(out)

}


#resilience
resil <- function (x) {
  mid <- round(nrow(x)/2) + 1
  return(sum(x[1:(mid-1),"performance"])/sum(x[1:(mid+1),"performance"]))
}

#resistance
resist <- function (x) {
  mid <- round(nrow(x)/2) + 1
  return(x[mid,"performance"]/sum(x[1:(mid-1),"performance"]))
}

#recovery
recov <- function (x) {
  mid <- round(nrow(x)/2) + 1
  return(sum(x[1:(mid+1),"performance"])/sum(x[mid,"performance"]))
}

