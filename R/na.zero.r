
#' @title Replace NA by 0
#'
#' @param x An array or matrix
#'
#' @export na.zero
#'
#' @examples
#' x <- c(-1,0,1,0,NA,0,1,1)
#' na.zero(x)
#' x <- matrix(x, ncol=2)
#' na.zero(x)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}