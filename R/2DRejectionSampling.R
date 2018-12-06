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
    sim_data <- sim_data[!is.na(sim_data)]
    for (i in 1:100){
      if (length(sim_data) >= n) {
        sim_data <- sim_data[1:n]
        break
      }
      else if (length(sim_data < n)) {
        percent <- length(sim_data)/10000
        sim_data2 <- loop_again(pdf,a,b, C, percent) #loops over the sim_dat function 1/percent times
        sim_data <- c(sim_data,sim_data2[!is.na(sim_data2)])
        #sim_data <- sim_data[!is.na(sim_data)]
      }
    }
    sim_data
}

loop_again <- function(pdf,a,b,C, percent){
  sim_data2D <- replicate(1000/percent,{
    v <- runif(2,a,b)
    u <- runif(1,0,C)
    if(u < jpdf(v)){
      sample <- data.frame( x = v[1],
                            y = v[2])
      } else {
        NA
        }
    })
  sim_data2D
  }


