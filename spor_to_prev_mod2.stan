data {
  int<lower=0> N;
  vector[N] spor_mean;
  int<lower=0,upper=1> prev_mean[N];
}
parameters {
real<lower=0, upper=1> alpha;
real<lower=0, upper=1>  beta;
real<lower=0, upper=0.9> sigma;
real<lower=0, upper=6> delta;
}
model {
for (n in 1:N)
prev_mean[n] ~ bernoulli_logit((alpha * pow(spor_mean[n],sigma))/(delta + beta * pow(spor_mean[n],sigma)));
}
