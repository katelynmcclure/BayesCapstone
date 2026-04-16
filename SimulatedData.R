set.seed(454)  # for reproducibility
n <- 100 # sample size

# generate predictors

# x1: strong signal
x1 <- rnorm(n, mean = 0, sd = 1) # N(0,1)
# x2: correlated with x1 with noise
x2 <- 0.85 * x1 + rnorm(n, mean = 0, sd = 0.5)
# x3: moderate signal
x3 <- rnorm(n, mean = 0, sd = 1)
# x4: pure noise
x4 <- rnorm(n, mean = 0, sd = 1)

epsilon <- rnorm(n, mean = 0, sd = 2.5) # generate noise term N(0, 2.5^2)
y = 2 + 2*x1 + 0.8*x3 + epsilon # true model
sim_data <- data.frame(y, x1, x2, x3, x4) # combine into dataset
# head(sim_data)

cor(x1, x2) # testing to see that x1 and x2 actually are correlated
cor(x1, x3) # testing to see that x1 and x3 are not that correlated

summary(lm(y ~ x1 + x3, data = sim_data)) # sanity check: fit the true model, x1 and x3 highly significant
summary(lm(y ~ x1 + x2, data = sim_data)) # sanity check: misleading model

saveRDS(sim_data, "sim_data.rds")
