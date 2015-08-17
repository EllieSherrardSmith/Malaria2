data {
  int<lower=0> N;  //Number of groups
  int<lower=0> N_dat; //Number of oocyst means in each group

  // Raw data counts of the oocysts for each group 
  int<lower=0> ooc_dat[N_dat, N];

  // Raw data counts of the sporos for each group
  int<lower=0> spor_dat[N_dat, N];
}

parameters {
  real<lower=0, upper=1> alpha;
  real<lower=0.1, upper=1>  beta;
  real<lower=0.3, upper=0.9> sigma;
  real<lower=2, upper=6> delta;
  real<lower=0> eps;
}

model {
  for (n in 1:N_dat)
    spor_dat[n] ~ normal((alpha * pow(ooc_dat[n],sigma))/(delta + beta * pow(ooc_dat[n],sigma)), eps);
}