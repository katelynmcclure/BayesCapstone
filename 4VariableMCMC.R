set.seed(123)
p <- 4  # number of predictors
# start at a model (x1 and x3 included)
gamma_current <- c(1, 0, 1, 0)
n_iter <- 5000
chain <- matrix(NA, n_iter, p) # 5000 row, 4 column matrix
chain[1, ] <- gamma_current # set first value
# posterior score 
log_post <- function(gamma){ # gamma = binary vector
  vars <- c("x1","x2","x3","x4")[gamma == 1] # translate to variable names for lm formula
  if (length(vars) == 0) {
    fit <- lm(y ~ 1, data = sim_data) # intercept model
  } else {
    form <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
    fit <- lm(form, data = sim_data)
  }
  -0.5 * BIC(fit) # logP(Mk | D) is approx -.5*BIC, Bayesian approximation
}
# MARKOV CHAIN!
for (i in 2:n_iter) {
  # propose: flip one variable 
  gamma_prop <- gamma_current
  j <- sample(1:p, 1) # random number 1-p
  gamma_prop[j] <- 1 - gamma_prop[j] # 1 - 1(currently included) = 0 (excluded), 1 - 0 (currently excluded) = 1 (included)
  # acceptance probability, Is this new model more likely (posterior probability) than the current model?
  alpha <- min(1, exp(log_post(gamma_prop) - log_post(gamma_current))) 
  # accept/reject 
  if (runif(1) < alpha) {
    gamma_current <- gamma_prop # accept proposal
  }
  chain[i, ] <- gamma_current # update chain
}

# posterior inclusion probability
pip_est <- colMeans(chain) # how often do the variables appear in the chain?

pip_df <- tibble(
  variable = paste0("x", 1:ncol(chain)),
  pip = pip_est
)

ggplot(pip_df, aes(x = variable, y = pip)) +
  geom_col(fill = "#de8984") +
  ylim(0, 1) +
  labs(
    title = "Posterior Inclusion Probabilities (MH Approx)",
    x = "Variable",
    y = "PIP"
  ) +
  theme_minimal()


# just like what BAS does under the hood!
library(BAS)
set.seed(123)
bas_fit <- bas.lm(y ~ x1 + x2 + x3 + x4, 
       data = sim_data, 
       method = "MCMC",               
       MCMC.iterations = 5000,       
       prior = "BIC", 
       modelprior = uniform())

pip_bas <- bas_fit$probne0 # extract PIP

pip_df_bas <- tibble(
  variable = c("Intercept", "x1", "x2", "x3", "x4"),
  pip = pip_bas
) %>%
  filter(variable != "Intercept")

ggplot(pip_df_bas, aes(x = variable, y = pip)) +
  geom_col(fill = "#e9d1f0") +
  ylim(0, 1) +
  labs(
    title = "Posterior Inclusion Probabilities (BAS MCMC)",
    x = "Variable",
    y = "PIP"
  ) +
  theme_minimal()
