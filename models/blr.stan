data {
  int<lower=1> T;             // Number of pre-treatment observations
  int<lower=0> T_new;         // Number of post-treatment observations
  vector[T] y;                // Pre-treatment outcome (e.g., log ridership)
  int<lower=1> K;             // Number of donor units/covariates
  matrix[T, K] X;             // Pre-treatment donor matrix
  matrix[T_new, K] X_new;     // Post-treatment donor matrix
}

parameters {
  real alpha;                 // Intercept (base level)
  vector[K] beta;             // Weights for donors/covariates
  real<lower=0> sigma_y;      // Observation noise
}

transformed parameters {
  vector[T] mu;
  mu = alpha + X * beta;
}

model {
  // Priors
  // Using a Normal prior for weights; for a "Classic" synthetic control, 
  // one might use a Dirichlet or positive constraints, but a 
  // regression-based approach is more flexible for log-transformed data.
  alpha ~ normal(0, 1);
  beta ~ normal(0, 0.5); 
  sigma_y ~ normal(0, 0.1);

  // Likelihood
  y ~ normal(mu, sigma_y);
}

generated quantities {
  vector[T] y_rep;
  vector[T_new] y_forecast;
  vector[T] log_lik;

  // In-sample reconstruction
  for (t in 1:T) {
    y_rep[t] = normal_rng(alpha + X[t] * beta, sigma_y);
    log_lik[t] = normal_lpdf(y[t] | alpha + X[t] * beta, sigma_y);
  }

  // Out-of-sample counterfactual (The "Synthetic Control")
  for (t in 1:T_new) {
    y_forecast[t] = normal_rng(alpha + X_new[t] * beta, sigma_y);
  }
}