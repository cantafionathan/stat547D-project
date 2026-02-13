library(cmdstanr)

fit_gam <- function(dat, stan_file = "models/gam.stan", seed = 547) {
  
  stan_data <- list(
    T = length(dat$y_pre_centered),
    T_new = nrow(dat$X_post_scaled),
    y = dat$y_pre_centered,
    K = ncol(dat$X_pre_scaled),
    X = dat$X_pre_scaled,
    X_new = dat$X_post_scaled,
    S = ncol(dat$B_pre),
    B = as.matrix(dat$B_pre),
    B_new = as.matrix(dat$B_post)
  )
  
  model <- cmdstan_model(stan_file)
  
  fit <- model$sample(
    data = stan_data, 
    seed = seed,
    chains = 4, 
    parallel_chains = 4,
    refresh = 0
  )
  
  return(fit)
}