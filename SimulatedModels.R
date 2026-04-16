library(tidyverse)

sim_data <- readRDS("sim_data.rds")

preds <- c("x1", "x2", "x3", "x4")

# all 16 possible models
model_list <- map(0:length(preds), function(k) {
  if (k == 0) {
    list(character(0)) # intercept model
  } else {
    combn(preds, k, simplify = FALSE)
  }
}) %>%
  list_flatten() # change list of lists into vector

# clean model names
model_names <- map_chr(model_list, ~ {
  if (length(.x) == 0) {
    "intercept"
  } else {
    paste(.x, collapse = " + ")
  }
})

# fit models
models <- map(model_list, ~ {
  if (length(.x) == 0) {
    lm(y ~ 1, data = sim_data)
  } else {
    form <- as.formula(paste("y ~", paste(.x, collapse = " + ")))
    lm(form, data = sim_data)
  }
})

# extract BIC values for each model
bic_vals <- map_dbl(models, BIC)

# posterior model probabilities
log_post <- -0.5 * bic_vals
log_post <- log_post - max(log_post)  # numerical stability

post_unnormalized <- exp(log_post)
model_probs <- post_unnormalized / sum(post_unnormalized)

# results table
results <- tibble(
  model = model_names,
  bic = bic_vals,
  post_prob = model_probs
) %>%
  arrange(desc(post_prob))

# add predictions
pred_matrix <- map(models, ~ predict(.x, newdata = sim_data)) %>%
  bind_cols()

colnames(pred_matrix) <- model_names

pred_matrix <- as_tibble(pred_matrix)

# BMA
bma_pred <- pred_matrix %>%
  as.matrix() %*% model_probs

sim_data <- sim_data %>%
  mutate(bma_pred = as.numeric(bma_pred))

# best single mode
best_model_index <- which.max(model_probs)

sim_data <- sim_data %>%
  mutate(best_model_pred = pred_matrix[[best_model_index]])

# compare performance
tibble(
  method = c("BMA", "Best Model"),
  mse = c(
    mean((sim_data$y - sim_data$bma_pred)^2),
    mean((sim_data$y - sim_data$best_model_pred)^2)
  )
)
