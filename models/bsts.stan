data {
  int<lower=1> T;
  int<lower=0> T_new;
  vector[T] y;
  int<lower=1> K;
  matrix[T, K] X;
  matrix[T_new, K] X_new;
}

parameters {
  vector[K] beta;
  real<lower=0> sigma_y;
  real<lower=0> sigma_trend;
  real<lower=0> sigma_season;

  vector[T] mu;      // Latent Level
  vector[T] season;  // Latent Seasonality
}

model {
  // Priors
  beta ~ normal(0, 0.5);
  sigma_y ~ normal(0, 0.1);
  sigma_trend ~ normal(0, 0.05);
  sigma_season ~ normal(0, 0.05);

  mu[1] ~ normal(y[1], 0.1);
  season[1:11] ~ normal(0, 0.2);

  for (t in 2:T) {
    // Local Level Transition (Random Walk)
    mu[t] ~ normal(mu[t-1], sigma_trend);
    
    // Seasonal Transition
    if (t >= 12) {
      season[t] ~ normal(-sum(season[(t-11):(t-1)]), sigma_season);
    }
  }

  // Observation equation
  y ~ normal(mu + season + X * beta, sigma_y);
}

generated quantities {
  vector[T] y_rep;
  vector[T_new] y_forecast;
  vector[T] log_lik;
  
  {
    real current_mu = mu[T];
    vector[11] season_buffer = season[(T-10):T]; 
    
    for (t in 1:T) {
      y_rep[t] = normal_rng(mu[t] + season[t] + X[t] * beta, sigma_y);
      log_lik[t] = normal_lpdf(y[t] | mu[t] + season[t] + X[t] * beta, sigma_y);
    }

    for (t in 1:T_new) {
      real pred_season = -sum(season_buffer);
      // In Local Level, the expected future mu is simply the last observed mu
      y_forecast[t] = normal_rng(current_mu + pred_season + X_new[t] * beta, sigma_y);
      
      // Update states for next step of forecast
      // (No current_delta to add here)
      for (i in 1:10) season_buffer[i] = season_buffer[i+1];
      season_buffer[11] = pred_season;
    }
  }
}