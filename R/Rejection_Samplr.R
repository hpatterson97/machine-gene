#' Add together two numbers
#' @description This is code that performs rejection sampling
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples n/a
#' @export


loop_again <- function(pdf,a,b,C, percent){
  print("test 2 -------")
  print(1/percent)
  print("end 2  -------")
  sim_data <- replicate(1000/percent, {
    u <- runif(1, a, b)
    v <- runif(1, 0, C)
    if(v < pdf(u)) {#then accept else reject
      u
    } else {
      NA
    }
  })
  sim_data
}

rejection_samplr <- function(n, pdf, a , b, C) {
    sim_data <- replicate(10000, {
      v <- runif(1,a,b)
      u <- runif(1,0,C)
      if(v < pdf(u)) {#then accept else reject
        u
        } else {
          NA
        }
      })
    sim_data <- sim_data[!is.na(sim_data)]
    print(n)
    for (i in 1:100){
      if (length(sim_data) >= n) {
        sim_data <- sim_data[1:n]
        break
        }
      else if (length(sim_data < n)) {
        percent <- length(sim_data)/10000
        print("test 1 -------")
        print(percent)
        print("end -------")
        sim_data2 <- loop_again(pdf,a,b, C, percent) #loops over the sim_dat function 1/percent times
        sim_data <- c(sim_data,sim_data2[!is.na(sim_data2)])
        #sim_data <- sim_data[!is.na(sim_data)]
        }
      }#throw in warning message in case doesn't give right number: r command warning
    sim_data
}

my_pdf <- function(x) {
  ifelse(x>=0 & x<=2, (x/2), 0)
}
#testing function
sim_data <- rejection_samplr(30000, my_pdf, 0,2,1)
head(sim_data)
length(sim_data)

# changes for creating new git directory


