data {
  int<lower=0> N;
  vector[N] ooc_mean;
  vector[N] prev_mean;
}
parameters {
  real<lower=0.1, upper=0.5> alpha;
  real<lower=0.1, upper=0.9>  beta;
  real<lower=0, upper=1> delta;
  real<lower=0> sigma;
}
model {
  for (n in 1:N)
    prev_mean[n] ~ normal((alpha/beta) * exp(-exp(delta - beta * ooc_mean[n])),sigma);
## not normal distribution (between 0 and 1)
}

