# Setting the seed for reproducibility
set.seed(1998)

ttimes <- c(0, 0, 0)

x <- rep(list(list()), 3)
sn <- rep(list(list()), 3)

flags <- c(FALSE, FALSE, FALSE)

while (TRUE) {

  xi1 <- rexp(n = 1, rate = 1)
  xi2 <- rexp(n = 1, rate = 3)
  xi3 <- rexp(n = 1, rate = 5)

  xi <- c(xi1, xi2, xi3)

  for (i in seq(1, 3)){
    if (flags[i] == FALSE && ttimes[i] + xi[i] <= 50){

      ttimes[i] <- ttimes[i] + xi[i]
      x[[i]] <- c(x[[i]], xi[i])
      sn[[i]] <- c(sn[[i]], ttimes[i])
    }
    else{
      flags[i] <- TRUE
    }
  }

  if(flags[1] == TRUE && flags[2] == TRUE && flags[3] == TRUE){
    break
  }

}

nt <- matrix(0, nrow = 3, ncol = 50)



for (i in seq(1, 3)) {
  for (j in seq(1, length(sn[[i]]) - 1)) {
    for (k in seq(1, 50)) {
      if(sn[[i]][j] <= k) {
        nt[i, k] <- j
      }
    }
  }
}

plot(unlist(sn[[3]]), type = "l", col="red", main = "Sum of inter-arrival times Vs. Number of days", xlab = "Number of Arrivals", ylab = "Sum of inter-arrival times")
lines(unlist(sn[[2]]), type = "l", col="blue")
lines(unlist(sn[[1]]), type = "l", col="green")

legend("bottomright", legend = c("Texas (lamda = 1)", "Florida (lamda = 2)", "Colorado (lamda = 3)"), col = c("green", "blue", "red"), pch=c(1,1,1))

plot(nt[1, ] + nt[2, ] + nt[3, ], type = "l", col="black", main = "Number of accidents Vs. Number of days", xlab = "Number of days", ylab = "Number of Accidents")
lines(nt[3, ], type = "l", col="red")
lines(nt[2, ], type = "l", col="blue")
lines(nt[1, ], type = "l", col="green")
legend("topleft", legend = c("Texas (lamda = 1)", "Florida (lamda = 2)", "Colorado (lamda = 3)", "Total"), col = c("green", "blue", "red", "black"), pch=c(1,1,1,1))
