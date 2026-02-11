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
  X_new = X_post_scaled,
  S = ncol(B_pre),
  B = as.matrix(B_pre),
  B_new = as.matrix(B_post)
)

# Run Model
model <- cmdstan_model("models/gam.stan")
gam_fit <- model$sample(data = stan_data, chains = 4, parallel_chains = 4)

# Plotting
df_pre <- get_intervals(as_draws_matrix(gam_fit$draws("y_rep")), dates_pre, y_mean)
df_post <- get_intervals(as_draws_matrix(gam_fit$draws("y_forecast")), dates_post, y_mean)
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
  labs(title = "GAM Counterfactual", y = "Log UPT") + theme_minimal()

# Diagnostics for Master Table
gam_loo <- gam_fit$loo()
y_rep_mean <- colMeans(as_draws_matrix(gam_fit$draws("y_rep")))

gam_results <- list(
  model_name = "GAM (Non-linear)",
  elpd = gam_loo$estimates["elpd_loo", "Estimate"],
  elpd_se = gam_loo$estimates["elpd_loo", "SE"],
  loo_obj = gam_loo,
  rmse_pre = sqrt(mean((y_pre_centered - y_rep_mean)^2)),
  portland_weight = gam_fit$summary("beta")$mean[1],
  ate = {
    y_fore_mean <- colMeans(as_draws_matrix(gam_fit$draws("y_forecast"))) + y_mean
    y_obs_post <- y_all[post_idx][1:length(y_fore_mean)]
    mean(y_obs_post - y_fore_mean, na.rm = TRUE)
  },
  spec_param = gam_fit$summary("tau")$mean
)


# 1. Extract the posterior draws for the forecast
# y_forecast is in the generated quantities of your Stan file
y_fore_draws <- as_draws_matrix(gam_fit$draws("y_forecast")) # iterations x time_points

# 2. Re-center the forecast (add the mean back)
y_fore_uncentered <- y_fore_draws + y_mean

# 3. Get the observed post-treatment data
y_obs_post <- y_all[post_idx][1:ncol(y_fore_uncentered)]

# 4. Calculate the Treatment Effect (Lift) for every iteration
# We subtract the counterfactual from the observed
# (iterations x time_points)
lift_draws_log <- sweep(y_fore_uncentered, 2, y_obs_post, FUN = "-") * -1

# 5. Calculate the Average Treatment Effect (ATE) per iteration
ate_draws_log <- rowMeans(lift_draws_log)

# 6. CALCULATE NUMBERS FOR PAPER:
# A. Mean Log ATE and Credible Intervals
ate_log_mean <- mean(ate_draws_log)
ate_log_ci <- quantile(ate_draws_log, probs = c(0.025, 0.975))

# B. Percentage Lift (This is usually what people report)
# Formula: 100 * (exp(log_diff) - 1)
ate_pct_draws <- (exp(ate_draws_log) - 1) * 100
ate_pct_mean <- mean(ate_pct_draws)
ate_pct_ci <- quantile(ate_pct_draws, probs = c(0.025, 0.975))

# C. Probability of Lift > 0 (The "Bayesian p-value")
prob_positive <- sum(ate_draws_log > 0) / length(ate_draws_log)

# D. Absolute Trip Lift (for the final month in your data)
final_idx <- ncol(y_fore_uncentered)
final_obs_raw <- exp(y_obs_post[final_idx])
final_counterfactual_raw_mean <- mean(exp(y_fore_uncentered[, final_idx]))
abs_lift <- final_obs_raw - final_counterfactual_raw_mean

# Print for results section:
cat(sprintf("Log ATE: %.3f [%.3f, %.3f]\n", ate_log_mean, ate_log_ci[1], ate_log_ci[2]))
cat(sprintf("Pct Lift: %.1f%% [%.1f%%, %.1f%%]\n", ate_pct_mean, ate_pct_ci[1], ate_pct_ci[2]))
cat(sprintf("Prob(Lift > 0): %.4f\n", prob_positive))
cat(sprintf("Abs Lift (Final Month): %.0f trips\n", abs_lift))