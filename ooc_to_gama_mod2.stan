data {
int<lower=0> N;
vector[N] ooc_mean;
vector[N] gama_mean;
}
parameters {
real<lower=0, upper=1> alpha;
real<lower=0, upper=1>  beta;
real<lower=1, upper=6> delta;
real eps;
}
model {
for (n in 1:N)
gama_mean[n] ~ normal((alpha/beta) * exp(-exp(delta-beta*ooc_mean[n])),eps);
}



