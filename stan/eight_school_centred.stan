// Centred 8 School Model
data {
  int<lower=0> J; // N schools
  real y[J]; // treatment indexed by J
  real<lower=0> sigma[J]; // s.e. indexed by J
}
parameters {
  real mu;
  real<lower=0> tau;
  real theta[J];
}
model {
  // Our "population" priors
  mu ~ normal(0, 5); // location
  tau ~ cauchy(0, 5); // scale
  
  // The "lower rung" of the hierarchy ladder
  theta ~ normal(mu, tau); 
  
  // Our likelihood:
  y ~ normal(theta, sigma); 
}
