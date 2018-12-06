#' @description This is code that performs rejection sampling on a continuous random variable
#' @param n the number of samples
#' @param jpdf a function that is the joint pdf of the random variable from which you want to sample. Should use x and y.
#' @param a a numeric that is the lower bound of the random variable you wish to sample from
#' @param b a numeric that is the upper bound of the random variable you wish to sample from
#' @param C a numeric that is such that f(x) ≤ C for all values of x and y
#' @return a random sample of coordinate pairs
samplr2D <- function(n, jpdf, a , b, C) {
  sim_data2D <- replicate(10000,{
    v <- runif(2,a,b)
    u <- runif(1,0,C)
    if(u < jpdf(v)){
      sample <- data.frame( x = v[1],
                            y = v[2])
     } else {
       NA
    }
})
}

samplr2d()
