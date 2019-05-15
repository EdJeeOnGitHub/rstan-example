// Non Centred 8 School Model
data {
  int<lower=0> J; // N schools
  real y[J]; // treatment indexed by J
  real<lower=0> sigma[J]; // s.e. indexed by J
}
parameters {
  real mu;
  real theta_tilde[J]; // this will be an intermediate param
  real<lower=0> tau;
}

transformed parameters {
  real theta[J];
  for (j in 1:J){
    theta[j] = mu + tau * theta_tilde[j]; // theta's final form
  }
}

model {
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 5);
  
  theta_tilde ~ normal(0, 1); // Will be N(mu, tau) after transform
  
  y ~ normal(theta, sigma);
}
