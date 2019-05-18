data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors
  matrix[N, K] x; // predictor matrix
  vector[N] y; // outcome vector
}
parameters {
  vector[K] beta; // coefficients for predictors
  real<lower=0> sigma; // error scale
  real alpha; // intercept
}
model {
  beta ~ normal(0, 100); // prior on betas
  sigma ~ normal(0, 100);
  alpha ~ normal(0, 100);
  y ~ lognormal(alpha + x * beta, sigma); // likelihood
}
