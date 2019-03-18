# Author: Aditya Chetan (2016217)

# Initialising the stake
stake <- 10

# Array initiaised to keep record of stake
record <- stake

# Start infinite iteration
while (TRUE) {
  
  # Sample a random number between 0 and 1
  toss <- runif(n = 1)
  
  # If random number is less than 0.5 step backwards
  if (toss < 0.5) {
    stake <- stake - 1
  }
  # If random number is more than 0.5 step forward
  else {
    stake <- stake + 1
  }
  
  # Append to the record array
  record <- c(record, stake)
  
  # If you reach the boundary, i.e. 0 and 20, exit
  if (stake == 0 || stake == 20) {
    break
  }
   
}

# Plot the records of stakes Vs. the number of steps
plot(x = seq(1, length(record)), y = record, type="l", xlab = "Time Index", ylab = "Stake", main = "Realization of a symmetric random walk")