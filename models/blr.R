library(cmdstanr)

fit_blr <- function(dat, stan_file = "models/blr.stan", seed = 547) {
  
  # Prepare Stan Data from the processed data object
  stan_data <- list(
    T = length(dat$y_pre_centered),
    T_new = nrow(dat$X_post_scaled),
    y = dat$y_pre_centered,
    K = ncol(dat$X_pre_scaled),
    X = dat$X_pre_scaled,
    X_new = dat$X_post_scaled
  )
  
  # Compile and Run
  model <- cmdstan_model(stan_file)
  
  fit <- model$sample(
    data = stan_data, 
    seed = seed,
    chains = 4, 
    parallel_chains = 4,
    iter_warmup = 1000, 
    iter_sampling = 1000,
    refresh = 0 # Reduce console clutter
  )
  
  return(fit)
}