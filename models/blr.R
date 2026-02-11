source("models/data-loader.R")
library(cmdstanr)
library(posterior)
library(loo)

set.seed(547)

# Prepare Stan Data
stan_data <- list(
  T = length(y_pre_centered),
  T_new = nrow(X_post_scaled),
  y = y_pre_centered,
  K = ncol(X_pre_scaled),
  X = X_pre_scaled,
  X_new = X_post_scaled
)

# Run Model
model <- cmdstan_model("models/blr.stan")
blr_fit <- model$sample(data = stan_data, chains = 4, parallel_chains = 4, 
                    iter_warmup = 1000, iter_sampling = 1000)

# Plotting
df_pre <- get_intervals(as_draws_matrix(blr_fit$draws("y_rep")), dates_pre, y_mean)
df_post <- get_intervals(as_draws_matrix(blr_fit$draws("y_forecast")), dates_post, y_mean)
df_obs <- tibble(date = wide_data$date, observed = y_all)

ggplot() +
  geom_ribbon(data = df_pre, aes(date, ymin = lo90, ymax = hi90), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(data = df_post, aes(date, ymin = lo90, ymax = hi90), fill = "firebrick", alpha = 0.2) +
  geom_ribbon(data = df_pre, aes(date, ymin = lo50, ymax = hi50), fill = "steelblue", alpha = 0.25) +
  geom_ribbon(data = df_post, aes(date, ymin = lo50, ymax = hi50), fill = "firebrick", alpha = 0.25) +
  geom_line(data = df_pre, aes(date, mean), color = "steelblue") +
  geom_line(data = df_post, aes(date, mean), color = "firebrick", linetype = "dashed") +
  geom_point(data = df_obs, aes(date, observed), alpha = 0.5) +
  geom_vline(xintercept = TREATMENT_DATE, linetype = "dotted") +
  labs(title = "Bayesian Linear Regression Counterfactual", y = "Log UPT") + 
  theme_minimal()

# Diagnostics for Master Table
blr_loo <- blr_fit$loo()
y_rep_mean <- colMeans(as_draws_matrix(blr_fit$draws("y_rep")))

blr_results <- list(
  model_name = "BLR (Linear)",
  elpd = blr_loo$estimates["elpd_loo", "Estimate"],
  elpd_se = blr_loo$estimates["elpd_loo", "SE"],
  loo_obj = blr_loo,
  rmse_pre = sqrt(mean((y_pre_centered - y_rep_mean)^2)),
  portland_weight = blr_fit$summary("beta")$mean[1],
  ate = {
    y_fore_mean <- colMeans(as_draws_matrix(blr_fit$draws("y_forecast"))) + y_mean
    y_obs_post <- y_all[post_idx][1:length(y_fore_mean)]
    mean(y_obs_post - y_fore_mean, na.rm = TRUE)
  },
  spec_param = NA 
)