#' @description This is code that performs rejection sampling on a continuous random variable
#' @param n the number of samples
#' @param jpdf a function that is the joint pdf of the random variable from which you want to sample. Should use x and y.
#' @param a a numeric that is the lower bound of the random variable you wish to sample from
#' @param b a numeric that is the upper bound of the random variable you wish to sample from
#' @param C a numeric that is such that f(x) ≤ C for all values of x and y
#' @examples
#' ## sampling from two unif(0,1)
#'  unif_pdf <- function(x,y, min = 0, max = 1){
#'  x <- dunif(x, min = min, max = max)
#'  y <- dunif(y, min = min, max = max)
#'  x*y
#'  }
#'  test <- samplr2D(n = 2000, jpdf = unif_pdf, a = 0, b = 1, C = 1)
#' @return a data frame with x and y as the column names and the rows filled with accepted values
samplr2D <- function(n, jpdf, a , b, C) {
  sim_data2D <-  data.frame(x = numeric(0), y = numeric(0))
  for( i in 1:(3*n)) {
    v <- runif(2,a,b)
    u <- runif(1,0,C)
    if(u < jpdf(v[1],v[2])){
      sim_data2D <- rbind(sim_data2D, data.frame(x = v[1], y = v[2]))
    }
  }
  #print ("------------")
  #print(nrow(sim_data2D) )
  for (i in 1:100){
    if (nrow(sim_data2D) >= n) {
      sim_data2D <- sim_data2D[1:n,1:2]
      break
    }
    else if (nrow(sim_data2D) < n) {
      sim_data_two <- loop_again(n, jpdf,a,b,C) #loops over the sim_dat function 1/percent times
      sim_data2D<- rbind(sim_data2D,sim_data_two)
    }
  }
  sim_data2D
}


loop_again <- function(n,jpdf,a,b,C){
  sample <-  data.frame(x = numeric(0), y = numeric(0))
  for( i in 1:n) {
    v <- runif(2,a,b)
    u <- runif(1,0,C)
    if(u < jpdf(v[1],v[2])){
      sample <- rbind(sample, data.frame(x = v[1], y = v[2]))
    }
  sample
  }}









