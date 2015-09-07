// negative binomial parameterized as eta (log(mu)) and dispersion (phi)
// see p286 in stan-reference-2.4.0.pdf
// a basic GLM example
data {
  int<lower=1> N;    // rows of data
  vector[N] x;       // predictor
  int<lower=0> y[N]; // response
}
parameters {
  real<lower=0> phi; // neg. binomial dispersion parameter
  real b0;  // intercept
  real b1;  // slope
}
model {
  // priors:
  phi ~ cauchy(0, 3);
  b0 ~ normal(0, 5);
  b1 ~ normal(0, 5);
  // data model:
  y ~ neg_binomial_2_log(b0 + b1 * x, phi);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] <- neg_binomial_2_log_log(y[n], b0 + b1 * x[n], phi);
  }
}