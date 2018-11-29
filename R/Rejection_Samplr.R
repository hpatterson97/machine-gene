#' Add together two numbers
#' @description This is code that performs rejection sampling
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples n/a
#' @export


my_pdf <- function(x) {
  ifelse(x>=0 & x<=2, (x/2), 0)
}
