#' @description This is code that performs rejection sampling on a continuous random variable
#' @param n the number of samples
#' @param jpdf a function that is the joint pdf of the random variable from which you want to sample. Should use x and y.
#' @param a a numeric that is the lower bound of the random variable you wish to sample from
#' @param b a numeric that is the upper bound of the random variable you wish to sample from
#' @param C a numeric that is such that f(x) ≤ C for all values of x and y
#' @return a random sample of coordinate pairs
samplr2D <- function(n, jpdf, a , b, C) {
  v <- runif(2,a,b)
  u <- runif(1,0,C)
  sim_data2D <- jpdf(v) < u
}

v <- runif(2,0,1)
v[1]
