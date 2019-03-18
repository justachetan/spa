# Here I have used the fact that
# if we want to study Males and Females together,
# we can model them as a single poisson process with
# rate as 2 + 3 = 5

# Setting the seed for reproducibility
set.seed(2009)

ttimes <- c(0, 0, 0)

x <- rep(list(list()), 3)
sn <- rep(list(list()), 3)

flags <- c(FALSE, FALSE, FALSE)

while (TRUE) {
  
  xi1 <- rexp(n = 1, rate = 2)
  xi2 <- rexp(n = 1, rate = 3)
  xi3 <- rexp(n = 1, rate = 5)
  
  xi <- c(xi1, xi2, xi3)
  
  for (i in seq(1, 3)){
    if (flags[i] == FALSE && ttimes[i] + xi[i] <= 20){
      
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

nt <- matrix(0, nrow = 3, ncol = 20)



for (i in seq(1, 3)) {
  for (j in seq(1, length(sn[[i]]) - 1)) {
    for (k in seq(1, 20)) {
      if(sn[[i]][j] <= k) {
        nt[i, k] <- j
      }
    }
  }
}

plot(unlist(sn[[3]]), type = "l", col="red", main = "Sum of inter-arrival times Vs. Number of months", xlab = "Number of months", ylab = "Sum of inter-arrival times")
lines(unlist(sn[[2]]), type = "l", col="blue")
lines(unlist(sn[[1]]), type = "l", col="green")

legend("bottomright", legend = c("Males (lamda = 2)", "Females (lamda = 3)", "Combined (lamda = 5)"), col = c("green", "blue", "red"), pch=c(1,1,1))

plot(nt[3, ], type = "l", col="red", main = "Number of patients Vs. Number of months", xlab = "Number of months", ylab = "Number of Patients")
lines(nt[2, ], type = "l", col="blue")
lines(nt[1, ], type = "l", col="green")
legend("topleft", legend = c("Males (lamda = 2)", "Females (lamda = 3)", "Combined (lamda = 5)"), col = c("green", "blue", "red"), pch=c(1,1,1,1))
