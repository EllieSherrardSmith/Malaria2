data {
int<lower=0> N;
vector[N] ooc_mean;
vector[N] spor_mean;
}
parameters {
real<lower=0, upper=1> alpha;
real<lower=0.1, upper=1>  beta;
real<lower=0.3, upper=0.9> sigma;
real<lower=2, upper=6> delta;
real<lower=0> eps;
}
model {
for (n in 1:N)
spor_mean[n] ~ normal((alpha * pow(ooc_mean[n],sigma))/(delta + beta * pow(ooc_mean[n],sigma)), eps);
}



