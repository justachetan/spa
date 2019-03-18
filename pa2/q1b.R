# Author: Aditya Chetan (2016217)


# Importing MASS package for mvrnorm
library(MASS)


# Initialising the time intervals
t <- seq(0.001, 1, by = 0.001)

# Initialising the covariance matrix as a zero matrix
# having dimensions epochs x epochs
sigma = matrix(rep(x = 0, length(t)*length(t)), nrow = length(t), ncol = length(t))


# Populating the covariance matrix with
# the appropriate covariances. In this case,
# the kernel function is,
#       K(s, t) = min(s, t)
#, where s is the row and t is the column
for (row in seq(1, length(t))) {
  for (col in seq(1, length(t))) {
    sigma[row, col] <- min(t[row], t[col])
  }
}

# Setting up the zero mean vector
mu <- rep(0, length(t))

# Producing samples from the Multivariate Normal Distribution 
X_t <- mvrnorm(mu = mu, Sigma = sigma)

# Plotting the data obtained. X-axis is the time steps, 
# Y-axis is the value of the Gaussian process
plot(x = t, y = X_t, type="l", ylab = "X(t)", xlab = "t", main = "Part 1 (b)")



