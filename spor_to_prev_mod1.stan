data {
  int<lower=0> N;
  vector[N] spor_mean;
  int<lower=0,upper=1> inf[N];
}

parameters {
  vector[2] beta_theta;
  real alpha_theta;
}

model {
 
  int log_mu[N];   // Log mean and log precision of group mean sporozoite score NegBin distribution
  real log_phi[N];
  vector[N] logit_theta; // Logit infection probability for each control/treatment group
  
  for (n in 1:N) {

    // GLM of infection probability in place of intractable convolution
    logit_theta[n] <- beta_theta[1] * log_mu[n] + beta_theta[2] * log_phi[n] + alpha_theta;
    
    // Censored measurement
    spor_mean[n] ~ neg_binomial_log(log_mu, log_phi);

  }
  // Mice infection prevalence measurements
  for(n in 1:N) {
    inf[n] ~ bernoulli_logit(logit_theta);
  }
}