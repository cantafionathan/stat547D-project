data {
  int<lower=1> T;             // Pre-treatment observations
  int<lower=0> T_new;         // Post-treatment observations
  vector[T] y;                // Outcome
  
  // Linear Covariates (Donor Cities/Controls)
  int<lower=1> K;             
  matrix[T, K] X;             
  matrix[T_new, K] X_new;     

  // Spline Basis (for the temporal trend)
  int<lower=1> S;             // Number of basis functions (knots)
  matrix[T, S] B;             // Basis matrix pre-treatment
  matrix[T_new, S] B_new;     // Basis matrix post-treatment
}

parameters {
  real alpha;                 // Intercept
  vector[K] beta;             // Linear weights (donors)
  vector[S] bs;               // Spline coefficients
  real<lower=0> tau;          // Smoothing parameter (prior scale for splines)
  real<lower=0> sigma_y;      // Observation noise
}

transformed parameters {
  vector[T] mu;
  // Mean = Intercept + Linear Donors + Non-linear Time Trend
  mu = alpha + X * beta + B * bs;
}

model {
  // Priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 0.5);
  sigma_y ~ normal(0, 0.1);  
  
  // Smoothing prior for spline coefficients
  tau ~ exponential(1);
  bs ~ normal(0, tau); 

  // Likelihood
  y ~ normal(mu, sigma_y);
}

generated quantities {
  vector[T] y_rep;
  vector[T_new] y_forecast;
  vector[T] log_lik;

  for (t in 1:T) {
    y_rep[t] = normal_rng(alpha + X[t] * beta + B[t] * bs, sigma_y);
    log_lik[t] = normal_lpdf(y[t] | alpha + X[t] * beta + B[t] * bs, sigma_y);
  }

  for (t in 1:T_new) {
    y_forecast[t] = normal_rng(alpha + X_new[t] * beta + B_new[t] * bs, sigma_y);
  }
}