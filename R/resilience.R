
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

  l <- do.call(sapply(data, subset_res, event), c)

  if ("resilience" %in% index) {
    out <- do.call(lapply(l, resil), rbind)
    out$event <- event
  } if ("resistance" %in% index) {
      out2 <- do.call(lapply(l, resist), rbind)
      out2$event <- event
  } if ("recovery" %in% index) {
      out3 <- do.call(lapply(l, recov), rbind)
      out3$event <- event
  }

  return(list(resilience = out, resistance = out2, recovery = out3))

}


select_res <-  subset(data,
                      data$temp > (event-ntemp-1) &
                        data$temp < (event+ntemp+1))

#resilience
resil <- function (x) {
  mid <- round(nrow(x)) + 1
  return(sum(x[1:(mid-1),"performance"])/sum(x[1:(mid+1),"performance"]))
}

#resistance
resist <- function (x) {
  mid <- round(nrow(x)) + 1
  return(x[mid,"performance"]/sum(x[1:(mid-1),"performance"]))
}

#recovery
recov <- function (x) {
  mid <- round(nrow(x)) + 1
  return(sum(x[1:(mid+1),"performance"])/sum(x[event,"performance"]))
}

