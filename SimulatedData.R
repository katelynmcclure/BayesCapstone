set.seed(454)  # for reproducibility
n <- 100 # sample size

# generate predictors

# x1: strong signal
x1 <- rnorm(n, mean = 0, sd = 1) # N(0,1)
# x2: correlated with x1 with noise
x2 <- 0.7 * x1 + rnorm(n, mean = 0, sd = 0.5)
# x3: moderate signal
x3 <- rnorm(n, mean = 0, sd = 1)
# x4: pure noise
x4 <- rnorm(n, mean = 0, sd = 1)

epsilon <- rnorm(n, mean = 0, sd = 2) # generate noise term N(0, 2^2)
y <- 2 + 3*x1 + 1.5*x3 + epsilon # true model
sim_data <- data.frame(y, x1, x2, x3, x4) # combine into dataset
# head(sim_data)
