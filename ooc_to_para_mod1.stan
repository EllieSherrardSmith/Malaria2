data {
int<lower=0> N;
vector[N] ooc_mean;
vector[N] para_mean;
}
parameters {
real<lower=0, upper=1> alpha;
real<lower=0, upper=1>  beta;
real<lower=0, upper=1> sigma;
real<lower=1, upper=6> delta;
real<lower=0> eps;
}
model {
for (n in 1:N)
para_mean[n] ~ normal((alpha * pow(ooc_mean[n],sigma))/(delta + beta * pow(ooc_mean[n],sigma)), eps);
}



