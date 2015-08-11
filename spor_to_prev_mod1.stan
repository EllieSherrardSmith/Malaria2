data {
  int<lower=0> N;
  vector[N] spor_mean;
  int<lower=0,upper=1> inf[N];
}
parameters {
real<lower=0> alpha;
real<lower=0>  beta;
real<lower=0> sigma;
real<lower=0> delta;
}
model {
for (n in 1:N)
inf[n] ~ bernoulli_logit((alpha * pow(spor_mean[n],sigma))/(delta + beta * pow(spor_mean[n],sigma)));
}