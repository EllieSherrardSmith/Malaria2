data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real<lower=0> alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma); //data model
  //increment_log_prob(-log(sigma));     //log prior for p(sigma) proportion to 1/sigma
  sigma ~ cauchy(0, 5);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] <- normal_log(y[n], x[n] * beta + alpha, sigma);
  }
}