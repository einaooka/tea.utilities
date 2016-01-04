
#' @title Identify values between two values
#'
#' @description Identify values between two values
#'
#' @param x A numeric value or an array of values or dates.
#' @param start A numeric value or a date.
#' @param end A numeric value or a date.
#'
#' @export isBetween
#'
#' @examples
#' isBetween(3:10, start=1, end=5)
#' isBetween(as.Date("2012-1-1"), startDate, endDate)

isBetween <- function(x, start, end){
  if (class(x) == "Date"){ flag <- (x >= as.Date(start) & x<= as.Date(end))
  } else { flag <- (x >= start & x<= end)}
  return(flag)
}
