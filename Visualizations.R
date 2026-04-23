library(tidyverse)

# ensure sorted order
plot_data <- results %>%
  arrange(post_prob) %>%
  mutate(
    model = factor(model, levels = model),
    highlight = post_prob == max(post_prob)
  )

# posterior model probabilities plot
ggplot(plot_data, aes(x = model, y = post_prob, fill = highlight)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#de8984", "FALSE" = "grey80")) +
  labs(
    title = "Posterior Model Probabilities",
    subtitle = "p = 4 predictors, K = 16 models",
    x = "Model",
    y = "Posterior Probability"
  ) +
  geom_hline(yintercept = 1/20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1/100, linetype = "dashed", color = "blue") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 7)
  )

plot_data <- results %>%
  mutate(
    ratio_to_best = post_prob / max(post_prob)
  ) %>%
  arrange(ratio_to_best) %>%
  mutate(model = factor(model, levels = model))

ggplot(plot_data, aes(x = model, y = ratio_to_best)) +
  geom_col(fill = "#de8984") +
  coord_flip() +
  geom_hline(yintercept = 1/20, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1/100, linetype = "dashed", color = "blue") +
  labs(
    title = "Posterior Model Support Relative to Best Model",
    x = "Model",
    y = "Posterior Probability / Best Model Probability"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7)
  )

