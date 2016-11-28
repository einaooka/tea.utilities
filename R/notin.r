
#' @title Not-In
#'
#' @description The negation of %in%, which returns a logical vector indicating if there is a non-match or not for its left operand.
#'
#' @param x vector or NULL: the values to be matched.
#' @param table vector or NULL: the values to be matched against.
#'
#' @export "%notin%"
#'
#' @examples
#' 1:10 %notin% c(1,3,5,9)

"%notin%" <- function(x, table){ !(match(x, table, nomatch = 0) > 0) }