data {
  int<lower=0> N;
  vector[N] para_mean;
  int<lower=0,upper=1> inf[N];
}
parameters {
real<lower=0, upper=1> alpha;
real<lower=0, upper=1>  beta;
real<lower=0, upper=0.9> sigma;
real<lower=0, upper=6> delta;
}
model {
for (n in 1:N)
inf[n] ~ bernoulli_logit((alpha * pow(para_mean[n],sigma))/(delta + beta * pow(para_mean[n],sigma)));
}
