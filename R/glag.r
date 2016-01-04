
#' @title Generalized Lag Function
#'
#' @description Compute a lagged version of a series or matrix, shifting the series by a given number.
#' The shift number can be either positive or negative. Appropriate number of NA's are added
#' either at the begging or end of the returned series so that it is the same length as the
#' original series.
#'
#' @param x An array.
#' @param k A shift number. Can be potitive or negative integer.
#'
#' @export glag
#'
#' @examples
#' x <- 1:10
#' glag(x, 3)
#' glag(MonthName(x), -3)
#' glag(matrix(1:24,nrow=6, ncol=4), 2)

glag <- function(x,k){

  # an array
  if (is.null(dim(x))){
    if (k > 0){
      x <- c(rep(NA, k), x[1:(length(x)-k)])
    } else if (k < 0){
      k <- -1 * k
      x <- c( x[(k+1):length(x)], rep(NA, k))
    }
  } else { # data frame and matrix
    if (k > 0){
      x[(k+1):nrow(x),] <- x[1:(nrow(x)-k),]
      x[1:k,] <- NA
    } else if (k < 0){
      k <- -1 * k
      x[1:(nrow(x)-k),] <- x[(k+1):nrow(x),]
      x[c((nrow(x)-k+1):nrow(x)),] <- NA
    }
  }
  return(x)
}
