#' @title Rejection Sampler
#' @description This is code that performs rejection sampling on a continuous random variable
#' @param n the number of samples
#' @param pdf a function that is the pdf of the random variable that you wish to sample from
#' @param a a numeric that is the lower bound of the random variable you wish to sample from
#' @param b a numeric that is the upper bound of the random variable you wish to sample from, so P(a ≤ X ≤ b) = 1
#' @param C a numeric that is such that f(x) ≤ C for all values of x
#' @return a random sample of size n from the rv with pdf provided
#' @examples
#' ## sampling from custom pdf
#'  my_pdf <- function(x) {
#'    ifelse(x>=0 & x<=2, (x/2), 0)}
#'  sim_data <- rejection_samplr(30000, my_pdf, 0,2,1)
#'  hist(sim_data, probability = TRUE)
#'  curve(my_pdf(x), col = "red", add = TRUE)
#' ## sampling from dunif
#'  sim_data <- rejection_samplr(1300, dunif, 0,1,2)
#'  hist(sim_data, probability = TRUE)
#'  curve(dunif(x,0,1), col = "red", add = TRUE)
#' ## sampling from beta
#'  sim_data <- rejection_samplr(1300, pdf_beta, a = 0,b = 1,C =1.5)
#'  pdf_beta <- function(x) {
#'    dbeta(x, 2, 3)
#'  }
#'  hist(sim_data, probability = TRUE)
#'  curve(dbeta(x,2,3), col = "red", add = TRUE)
#' @export
rejection_samplr <- function(n, pdf, a , b, C) {
  if(is.numeric(n)==FALSE) stop("'n' must be a number")
  if(is.numeric(a)==FALSE) stop("'a' must be a number")
  if(is.numeric(b)==FALSE) stop("'b' must be a number")
  if(is.numeric(C)==FALSE) stop("'C' must be a number")
  if (n < 0) stop("'n' must be non-negative")
  if (a > b) stop("a must be less than b")
  if (C < 0) stop("'C' must be non-negative")
    sim_data <- replicate(10000, {
      v <- runif(1,a,b)
      u <- runif(1,0,C)
      if(u < pdf(v)) {#then accept else reject
        v
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
  sim_data <- replicate(1000/percent, {
    v <- runif(1, a, b)
    u <- runif(1, 0, C)
    if(u < pdf(v)) {#then accept else reject
      v
    } else {
      NA
    }
  })
  sim_data
}





