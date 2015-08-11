data {
  // Number of control rounds/bites = 16 rounds 1 to 4 and bites 2 to 5
  // (as all ATV were 0 for bite 1)
  int<lower=0> N_C;
  
  // Number of ATV treatment rounds (16)
  int<lower=0> N_Tatv;

  // Number of ATV treatment rounds (16)
  int<lower=0> N_T4b7;

  // Number of ATV treatment rounds (16)
  int<lower=0> N_Tvacc;

  // Number of ATV treatment rounds (16)
  int<lower=0> N_Tatv_vacc;

  // Number of ATV treatment rounds (16)
  int<lower=0> N_T4b7_vacc;
  
  // Number of oocysts in each round (45) - reduced to 24
  // to get rid of NAs from some groups
  int<lower=0> N_ooc;
  
  // Number of mice (5)
  int<lower=0> N_mice;
  
  // Raw data counts of the oocysts for each group of controls
  int<lower=0> ooc_count_C[N_ooc, N_C];
  
  // Raw data counts of the oocysts for each group of ATV treatments
  int<lower=0> ooc_count_T[N_ooc, N_Tatv];
  
  // Raw data counts of the oocysts for each group of ATV treatments
  int<lower=0> ooc_count_T[N_ooc, N_T4b7];
  
  // Raw data counts of the oocysts for each group of ATV treatments
  int<lower=0> ooc_count_T[N_ooc, N_Tvacc];
  
  // Raw data counts of the oocysts for each group of ATV treatments
  int<lower=0> ooc_count_T[N_ooc, N_Tatv_vacc];
  
  // Raw data counts of the oocysts for each group of ATV treatments
  int<lower=0> ooc_count_T[N_ooc, N_T4b7_vacc];
  
  // Infection prevalence of each mouse in each control group
  int<lower=0> prev_C[N_mice, N_C];
  
  // Infection prevalence of each mouse in each ATV treatment group
  int<lower=0> prev_T[N_mice, N_Tatv];
  
  // Infection prevalence of each mouse in each ATV treatment group
  int<lower=0> prev_T[N_mice, N_T4b7];
  
  // Infection prevalence of each mouse in each ATV treatment group
  int<lower=0> prev_T[N_mice, N_Tvacc];
  
  // Infection prevalence of each mouse in each ATV treatment group
  int<lower=0> prev_T[N_mice, N_Tatv_vacc];
  
  // Infection prevalence of each mouse in each ATV treatment group
  int<lower=0> prev_T[N_mice, N_T4b7_vacc];
  
  // Sporozoite count binning
  int<lower=0> N_bin;
  int<lower=0> bin_edge[N_bin + 1];
  
  // Binned sporozoite counts for each control/treatment group
  int<lower=0> s_count_C[N_C, N_bin];
  int<lower=0> s_count_T[N_Tatv, N_bin];
  int<lower=0> s_count_T[N_T4b7, N_bin];
  int<lower=0> s_count_T[N_Tvacc, N_bin]
  int<lower=0> s_count_T[N_Tatv_vacc, N_bin]
  int<lower=0> s_count_T[N_T4b7_vacc, N_bin]
}

transformed data {
  int N_bite;
  int N_round;
  real offset;
  
  N_bite <- 4;
  N_round <- 4;
  offset <- 4;
}

parameters {
  real rho__log_mean_ooc_C;
  real<lower=0> tau_bite__log_mean_ooc_C;
  real rho_bite__log_mean_ooc_C[N_bite];
  real<lower=0> tau_round__log_mean_ooc_C;
  real rho_round__log_mean_ooc_C[N_round];
  
  real rho__log_od_ooc_C;
  real<lower=0> tau_bite__log_od_ooc_C;
  real rho_bite__log_od_ooc_C[N_bite];
  real<lower=0> tau_round__log_od_ooc_C;
  real rho_round__log_od_ooc_C[N_round];
  
  real rho__log_mean_ooc_Tatv;
  real<lower=0> tau_bite__log_mean_ooc_Tatv;
  real rho_bite__log_mean_ooc_Tatv[N_bite];
  real<lower=0> tau_round__log_mean_ooc_Tatv;
  real rho_round__log_mean_ooc_Tatv[N_round];
  
  real rho__log_od_ooc_Tatv;
  real<lower=0> tau_bite__log_od_ooc_Tatv;
  real rho_bite__log_od_ooc_Tatv[N_bite];
  real<lower=0> tau_round__log_od_ooc_Tatv;
  real rho_round__log_od_ooc_Tatv[N_round];

  real rho__log_mean_ooc_T4b7;
  real<lower=0> tau_bite__log_mean_ooc_T4b7;
  real rho_bite__log_mean_ooc_T4b7[N_bite];
  real<lower=0> tau_round__log_mean_ooc_T4b7;
  real rho_round__log_mean_ooc_T4b7[N_round];
  
  real rho__log_od_ooc_T4b7;
  real<lower=0> tau_bite__log_od_ooc_T4b7;
  real rho_bite__log_od_ooc_T4b7[N_bite];
  real<lower=0> tau_round__log_od_ooc_T4b7;
  real rho_round__log_od_ooc_T4b7[N_round];

  real rho__log_mean_ooc_Tvacc;
  real<lower=0> tau_bite__log_mean_ooc_Tvacc;
  real rho_bite__log_mean_ooc_Tvacc[N_bite];
  real<lower=0> tau_round__log_mean_ooc_Tvacc;
  real rho_round__log_mean_ooc_Tvacc[N_round];
  
  real rho__log_od_ooc_Tvacc;
  real<lower=0> tau_bite__log_od_ooc_Tvacc;
  real rho_bite__log_od_ooc_Tvacc[N_bite];
  real<lower=0> tau_round__log_od_ooc_Tvacc;
  real rho_round__log_od_ooc_Tvacc[N_round];

  real rho__log_mean_ooc_Tatv_vacc;
  real<lower=0> tau_bite__log_mean_ooc_Tatv_vacc;
  real rho_bite__log_mean_ooc_Tatv_vacc[N_bite];
  real<lower=0> tau_round__log_mean_ooc_Tatv_vacc;
  real rho_round__log_mean_ooc_Tatv_vacc[N_round];
  
  real rho__log_od_ooc_Tatv_vacc;
  real<lower=0> tau_bite__log_od_ooc_Tatv_vacc;
  real rho_bite__log_od_ooc_Tatv_vacc[N_bite];
  real<lower=0> tau_round__log_od_ooc_Tatv_vacc;
  real rho_round__log_od_ooc_Tatv_vacc[N_round];

  real rho__log_mean_ooc_Tatv_vacc;
  real<lower=0> tau_bite__log_mean_ooc_Tatv_vacc;
  real rho_bite__log_mean_ooc_Tatv_vacc[N_bite];
  real<lower=0> tau_round__log_mean_ooc_Tatv_vacc;
  real rho_round__log_mean_ooc_Tatv_vacc[N_round];
  
  real rho__log_od_ooc_T4b7_vacc;
  real<lower=0> tau_bite__log_od_ooc_T4b7_vacc;
  real rho_bite__log_od_ooc_T4b7_vacc[N_bite];
  real<lower=0> tau_round__log_od_ooc_T4b7_vacc;
  real rho_round__log_od_ooc_T4b7_vacc[N_round];
  
  // Slopes and intercepts of GLM modeling conversion from oocyst counts
  // to sporozoite counts.  Common slopes for control and treatment groups
  // as the underlying process should be independent of treatment.
  vector[2] beta_mu;
  vector[2] beta_phi;
  real alpha_mu;
  real alpha_phi;
  
  // Slopes and intercept of GLM modeling infection probability from
  // sporozite count distribution.  Again, common parameters for control
  // and treatment as the underlying process should be independent of
  // treatment.
  vector[2] beta_theta;
  real alpha_theta;
}

model {
  
  // Log mean and log precision of control group oocyst count NegBin distribution
  vector[N_C] log_mu_ooc_C;
  vector[N_C] log_phi_ooc_C;
  
  // Log mean and log precision of treatment group oocyst count NegBin distribution
  vector[N_T] log_mu_ooc_Tatv;
  vector[N_T] log_phi_ooc_Tatv;

  vector[N_T] log_mu_ooc_T4b7;
  vector[N_T] log_phi_ooc_T4b7;

  vector[N_T] log_mu_ooc_Tvacc;
  vector[N_T] log_phi_ooc_Tvacc;

  vector[N_T] log_mu_ooc_Tatv_vacc;
  vector[N_T] log_phi_ooc_Tatv_vacc;

  vector[N_T] log_mu_ooc_T4b7_vacc;
  vector[N_T] log_phi_ooc_T4b7_vacc;
  
  // Logit infection probability for each control/treatment group
  vector[N_C] logit_theta_C;
  vector[N_T] logit_theta_Tatv;
  vector[N_T] logit_theta_T4b7;
  vector[N_T] logit_theta_Tvacc;
  vector[N_T] logit_theta_Tatv_vacc;
  vector[N_T] logit_theta_T4b7_vacc;
  
  // Sporozoite count censoring model parameters
  real sum_p;
  vector[N_bin] p;
  vector[N_bin - 1] cdf;
  
  // Priors
  rho__log_mean_ooc_C ~ normal(0, 5);
  tau_bite__log_mean_ooc_C ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_C ~ normal(0, 1);
  tau_round__log_mean_ooc_C ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_C ~ normal(0, 1);
  
  rho__log_od_ooc_C ~ normal(0, 5);
  tau_bite__log_od_ooc_C ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_C ~ normal(0, 1);
  tau_round__log_od_ooc_C ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_C ~ normal(0, 1);
  
  rho__log_mean_ooc_Tatv ~ normal(0, 5);
  tau_bite__log_mean_ooc_Tatv ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_Tatv ~ normal(0, 1);
  tau_round__log_mean_ooc_Tatv ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_Tatv ~ normal(0, 1);
  
  rho__log_od_ooc_Tatv ~ normal(0, 5);
  tau_bite__log_od_ooc_Tatv ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_Tatv ~ normal(0, 1);
  tau_round__log_od_ooc_Tatv ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_Tatv ~ normal(0, 1);
  
  rho__log_mean_ooc_T4b7 ~ normal(0, 5);
  tau_bite__log_mean_ooc_T4b7 ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_T4b7 ~ normal(0, 1);
  tau_round__log_mean_ooc_T4b7 ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_T4b7 ~ normal(0, 1);
  
  rho__log_od_ooc_T4b7 ~ normal(0, 5);
  tau_bite__log_od_ooc_T4b7 ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_T4b7 ~ normal(0, 1);
  tau_round__log_od_ooc_T4b7 ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_T4b7 ~ normal(0, 1);

  rho__log_mean_ooc_Tvacc ~ normal(0, 5);
  tau_bite__log_mean_ooc_Tvacc ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_Tvacc ~ normal(0, 1);
  tau_round__log_mean_ooc_Tvacc ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_Tvacc ~ normal(0, 1);
  
  rho__log_od_ooc_Tvacc ~ normal(0, 5);
  tau_bite__log_od_ooc_Tvacc ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_Tvacc ~ normal(0, 1);
  tau_round__log_od_ooc_Tvacc ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_Tvacc ~ normal(0, 1);

  rho__log_mean_ooc_Tatv_vacc ~ normal(0, 5);
  tau_bite__log_mean_ooc_Tatv_vacc ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_Tatv_vacc ~ normal(0, 1);
  tau_round__log_mean_ooc_Tatv_vacc ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_Tatv_vacc ~ normal(0, 1);
  
  rho__log_od_ooc_Tatv_vacc ~ normal(0, 5);
  tau_bite__log_od_ooc_Tatv_vacc ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_Tatv_vacc ~ normal(0, 1);
  tau_round__log_od_ooc_Tatv_vacc ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_Tatv_vacc ~ normal(0, 1);

  rho__log_mean_ooc_T4b7_vacc ~ normal(0, 5);
  tau_bite__log_mean_ooc_T4b7_vacc ~ cauchy(0, 2.5);
  rho_bite__log_mean_ooc_T4b7_vacc ~ normal(0, 1);
  tau_round__log_mean_ooc_T4b7_vacc ~ cauchy(0, 2.5);
  rho_round__log_mean_ooc_T4b7_vacc ~ normal(0, 1);
  
  rho__log_od_ooc_T4b7_vacc ~ normal(0, 5);
  tau_bite__log_od_ooc_T4b7_vacc ~ cauchy(0, 2.5);
  rho_bite__log_od_ooc_T4b7_vacc ~ normal(0, 1);
  tau_round__log_od_ooc_T4b7_vacc ~ cauchy(0, 2.5);
  rho_round__log_od_ooc_T4b7_vacc ~ normal(0, 1);

  beta_mu ~ normal(0, 8);
  beta_phi ~ normal(0, 8);
  beta_theta ~ normal(0, 4);
  
  alpha_mu ~ normal(0, 8);
  alpha_phi ~ normal(0, 8);
  alpha_theta ~ normal(0, 4);
  
  // Oocyst measureuments (control)
  for (g in 1:N_C) {
    log_mu_ooc_C[g] <-   rho__log_mean_ooc_C
    + tau_bite__log_mean_ooc_C
    * rho_bite__log_mean_ooc_C[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_C
    * rho_round__log_mean_ooc_C[(g - 1) % N_round + 1];
    log_phi_ooc_C[g] <-  rho__log_od_ooc_C
    + tau_bite__log_od_ooc_C
    * rho_bite__log_od_ooc_C[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_C
    * rho_round__log_od_ooc_C[(g - 1) % N_round + 1];
  }
  
  for(n in 1:N_ooc)
    ooc_count_C[n] ~ neg_binomial_2_log(log_mu_ooc_C, exp(log_phi_ooc_C));
  
  // Oocyst measureuments (treatment_ATV)
  for (g in 1:N_Tatv) {
    log_mu_ooc_Tatv[g] <-   rho__log_mean_ooc_Tatv
    + tau_bite__log_mean_ooc_Tatv
    * rho_bite__log_mean_ooc_Tatv[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_Tatv
    * rho_round__log_mean_ooc_Tatv[(g - 1) % N_round + 1];
    log_phi_ooc_Tatv[g] <-  rho__log_od_ooc_Tatv
    + tau_bite__log_od_ooc_Tatv
    * rho_bite__log_od_ooc_Tatv[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_Tatv
    * rho_round__log_od_ooc_Tatv[(g - 1) % N_round + 1];
  }
  
  for(n in 1:N_ooc)
    ooc_count_Tatv[n] ~ neg_binomial_2_log(log_mu_ooc_Tatv, exp(log_phi_ooc_Tatv));
  
  // Oocyst measureuments (treatment_4B7)
  for (g in 1:N_T4b7) {
    log_mu_ooc_T4b7[g] <-   rho__log_mean_ooc_T4b7
    + tau_bite__log_mean_ooc_T4b7
    * rho_bite__log_mean_ooc_T4b7[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_T4b7
    * rho_round__log_mean_ooc_T4b7[(g - 1) % N_round + 1];
    log_phi_ooc_T4b7[g] <-  rho__log_od_ooc_T4b7
    + tau_bite__log_od_ooc_T4b7
    * rho_bite__log_od_ooc_T4b7[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_T4b7
    * rho_round__log_od_ooc_T4b7[(g - 1) % N_round + 1];
  }
  
  for(n in 1:N_ooc)
    ooc_count_T4b7[n] ~ neg_binomial_2_log(log_mu_ooc_T4b7, exp(log_phi_ooc_T4b7));

  // Oocyst measureuments (treatment_Vaccine)
  for (g in 1:N_Tvacc) {
    log_mu_ooc_Tvacc[g] <-   rho__log_mean_ooc_Tvacc
    + tau_bite__log_mean_ooc_Tvacc
    * rho_bite__log_mean_ooc_Tvacc[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_Tvacc
    * rho_round__log_mean_ooc_Tvacc[(g - 1) % N_round + 1];
    log_phi_ooc_Tvacc[g] <-  rho__log_od_ooc_Tvacc
    + tau_bite__log_od_ooc_Tvacc
    * rho_bite__log_od_ooc_Tvacc[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_Tvacc
    * rho_round__log_od_ooc_Tvacc[(g - 1) % N_round + 1];
  }
  
  for(n in 1:N_ooc)
    ooc_count_Tvacc[n] ~ neg_binomial_2_log(log_mu_ooc_Tvacc, exp(log_phi_ooc_Tvacc));

  // Oocyst measureuments (treatment_ATV & Vaccine)
  for (g in 1:N_Tatv_vacc) {
    log_mu_ooc_Tatv_vacc[g] <-   rho__log_mean_ooc_Tatv_vacc
    + tau_bite__log_mean_ooc_Tatv_vacc
    * rho_bite__log_mean_ooc_Tatv_vacc[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_Tatv_vacc
    * rho_round__log_mean_ooc_Tatv_vacc[(g - 1) % N_round + 1];
    log_phi_ooc_Tatv_vacc[g] <-  rho__log_od_ooc_Tatv_vacc
    + tau_bite__log_od_ooc_Tatv_vacc
    * rho_bite__log_od_ooc_Tatv_vacc[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_Tatv_vacc
    * rho_round__log_od_ooc_Tatv_vacc[(g - 1) % N_round + 1];
  }
  
  for(n in 1:N_ooc)
    ooc_count_Tatv_vacc[n] ~ neg_binomial_2_log(log_mu_ooc_Tatv_vacc, exp(log_phi_ooc_Tatv_vacc));

  // Oocyst measureuments (treatment 4B7 & Vaccine)
  for (g in 1:N_T4b7_vacc) {
    log_mu_ooc_T4b7_vacc[g] <-   rho__log_mean_ooc_T4b7_vacc
    + tau_bite__log_mean_ooc_T4b7_vacc
    * rho_bite__log_mean_ooc_T4b7_vacc[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_T4b7_vacc
    * rho_round__log_mean_ooc_T4b7_vacc[(g - 1) % N_round + 1];
    log_phi_ooc_T4b7_vacc[g] <-  rho__log_od_ooc_T4b7_vacc
    + tau_bite__log_od_ooc_T4b7_vacc
    * rho_bite__log_od_ooc_T4b7_vacc[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_T4b7_vacc
    * rho_round__log_od_ooc_T4b7_vacc[(g - 1) % N_round + 1];
  }
  
  for(n in 1:N_ooc)
    ooc_count_T4b7_vacc[n] ~ neg_binomial_2_log(log_mu_ooc_T4b7_vacc, exp(log_phi_ooc_T4b7_vacc));

  // Censored sporozoite measurements (control)
  for (n in 1:N_C) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_C[n]
    + beta_mu[2] * log_phi_ooc_C[n]
    + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_C[n]
    + beta_phi[2] * log_phi_ooc_C[n]
    + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_C[n] <-    beta_theta[1] * log_mu
    + beta_theta[2] * log_phi
    + alpha_theta;
    
    // Compute censored multinomial probabilities
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu), exp(log_phi));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_C[n] ~ multinomial(p);
    
  }
  
  // Censored sporozoite measurements (ATV treatment)
  // Treatment group distributions
  for (n in 1:N_Tatv) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_Tatv[n]
    + beta_mu[2] * log_phi_ooc_Tatv[n]
    + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_Tatv[n]
    + beta_phi[2] * log_phi_ooc_Tatv[n]
    + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_Tatv[n] <-   beta_theta[1] * log_mu
    + beta_theta[2] * log_phi
    + alpha_theta;
    
    // Compute censored multinomial probabilities
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu), exp(log_phi));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_Tatv[n] ~ multinomial(p);
  }

  // Censored sporozoite measurements (4B7 treatment)
  // Treatment group distributions
  for (n in 1:N_T4b7) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_T4b7[n]
    + beta_mu[2] * log_phi_ooc_T4b7[n]
    + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_T4b7[n]
    + beta_phi[2] * log_phi_ooc_T4b7[n]
    + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_T4b7[n] <-   beta_theta[1] * log_mu
    + beta_theta[2] * log_phi
    + alpha_theta;
    
    // Compute censored multinomial probabilities
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu), exp(log_phi));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_T4b7[n] ~ multinomial(p);
  }

  // Censored sporozoite measurements (Vaccine treatment)
  // Treatment group distributions
  for (n in 1:N_Tvacc) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_Tvacc[n]
    + beta_mu[2] * log_phi_ooc_Tvacc[n]
    + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_Tvacc[n]
    + beta_phi[2] * log_phi_ooc_Tvacc[n]
    + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_Tvacc[n] <-   beta_theta[1] * log_mu
    + beta_theta[2] * log_phi
    + alpha_theta;
    
    // Compute censored multinomial probabilities
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu), exp(log_phi));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_Tvacc[n] ~ multinomial(p);
  }

  // Censored sporozoite measurements (ATV & Vaccine treatment)
  // Treatment group distributions
  for (n in 1:N_Tatv_vacc) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_Tatv_vacc[n]
    + beta_mu[2] * log_phi_ooc_Tatv_vacc[n]
    + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_Tatv_vacc[n]
    + beta_phi[2] * log_phi_ooc_Tatv_vacc[n]
    + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_Tatv_vacc[n] <-   beta_theta[1] * log_mu
    + beta_theta[2] * log_phi
    + alpha_theta;
    
    // Compute censored multinomial probabilities
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu), exp(log_phi));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_Tatv_vacc[n] ~ multinomial(p);
  }

  // Censored sporozoite measurements (4B7 & Vaccine treatment)
  // Treatment group distributions
  for (n in 1:N_T4b7_vacc) {
    real log_mu;
    real log_phi;
    
    // GLM of sporozoite count distribution in place of intractable convolution
    log_mu <-  beta_mu[1] * log_mu_ooc_T4b7_vacc[n]
    + beta_mu[2] * log_phi_ooc_T4b7_vacc[n]
    + alpha_mu + offset;
    log_phi <-  beta_phi[1] * log_mu_ooc_T4b7_vacc[n]
    + beta_phi[2] * log_phi_ooc_T4b7_vacc[n]
    + alpha_phi + offset;
    
    // GLM of infection probability in place of intractable convolution
    logit_theta_T4b7_vacc[n] <-   beta_theta[1] * log_mu
    + beta_theta[2] * log_phi
    + alpha_theta;
    
    // Compute censored multinomial probabilities
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu), exp(log_phi));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    // Censored measurement
    s_count_T4b7_vacc[n] ~ multinomial(p);
  }


  // Mice infection prevalence measurements
  for(n in 1:N_mice) {
    prev_C[n] ~ bernoulli_logit(logit_theta_C);
    prev_Tatv[n] ~ bernoulli_logit(logit_theta_Tatv);
    prev_T4b7[n] ~ bernoulli_logit(logit_theta_T4b7);
    prev_Tvacc[n] ~ bernoulli_logit(logit_theta_Tvacc);
    prev_Tatv_vacc[n] ~ bernoulli_logit(logit_theta_Tatv_vacc);
    prev_T4b7_vacc[n] ~ bernoulli_logit(logit_theta_T4b7_vacc);
  }
  
}

generated quantities {
  int<lower=0> sim_ooc_count_C[N_C];
  int<lower=0> sim_ooc_count_Tatv[N_Tatv];
  int<lower=0> sim_ooc_count_T4b7[N_T4b7];
  int<lower=0> sim_ooc_count_Tvacc[N_Tvacc];
  int<lower=0> sim_ooc_count_Tatv_vacc[N_Tatv_vacc];
  int<lower=0> sim_ooc_count_T4b7_vacc[N_T4b7_vacc];

  int<lower=0> sim_s_count_C[N_C, N_bin];
  int<lower=0> sim_s_count_Tatv[N_Tatv, N_bin];
  int<lower=0> sim_s_count_T4b7[N_T4b7, N_bin];
  int<lower=0> sim_s_count_Tvacc[N_Tvacc, N_bin];
  int<lower=0> sim_s_count_Tatv_vacc[N_Tatv_vacc, N_bin];
  int<lower=0> sim_s_count_T4b7_vacc[N_T4b7_vacc, N_bin];
  
  real<lower=0, upper=1> theta_C[N_C];
  real<lower=0, upper=1> theta_Tatv[N_Tatv];
  real<lower=0, upper=1> theta_T4b7[N_T4b7];
  real<lower=0, upper=1> theta_Tvacc[N_Tvacc];
  real<lower=0, upper=1> theta_Tatv_vacc[N_Tatv_vacc];
  real<lower=0, upper=1> theta_T4b7_vacc[N_T4b7_vacc];

  real<lower=0, upper=1> theta_C_mosquito[N_C];
  real<lower=0, upper=1> theta_Tatv_mosquito[N_Tatv];
  real<lower=0, upper=1> theta_T4b7_mosquito[N_T4b7];
  real<lower=0, upper=1> theta_Tvacc_mosquito[N_Tvacc];
  real<lower=0, upper=1> theta_Tatv_vacc_mosquito[N_Tatv_vacc];
  real<lower=0, upper=1> theta_T4b7_vacc_mosquito[N_T4b7_vacc];
  
  int<lower=0, upper=N_mice> sim_prev_C[N_C];
  int<lower=0, upper=N_mice> sim_prev_Tatv[N_Tatv];
  int<lower=0, upper=N_mice> sim_prev_T4b7[N_T4b7];
  int<lower=0, upper=N_mice> sim_prev_Tvacc[N_Tvacc];
  int<lower=0, upper=N_mice> sim_prev_Tatv_vacc[N_Tatv_vacc];
  int<lower=0, upper=N_mice> sim_prev_T4b7_vacc[N_T4b7_vacc];
  
{
  real sum_p;
  vector[N_bin] p;
  vector[N_bin - 1] cdf;
  
  for (g in 1:N_C) {
    real log_mu_ooc;
    real log_phi_ooc;
    real log_mu_s;
    real log_phi_s;
    
    // Simulate oocyst count measurement
    log_mu_ooc <-  rho__log_mean_ooc_C
    + tau_bite__log_mean_ooc_C
    * rho_bite__log_mean_ooc_C[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_C
    * rho_round__log_mean_ooc_C[(g - 1) % N_round + 1];
    log_phi_ooc <-  rho__log_od_ooc_C
    + tau_bite__log_od_ooc_C
    * rho_bite__log_od_ooc_C[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_C
    * rho_round__log_od_ooc_C[(g - 1) % N_round + 1];
    
    sim_ooc_count_C[g]
    <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
    
    theta_C_mosquito[g] <- 1.0 - neg_binomial_2_cdf(0, exp(log_mu_ooc), exp(log_phi_ooc));
    
    // Simulate sporozoite count measurement
    log_mu_s <-  beta_mu[1] * log_mu_ooc
    + beta_mu[2] * log_phi_ooc
    + alpha_mu + offset;
    log_phi_s <-  beta_phi[1] * log_mu_ooc
    + beta_phi[2] * log_phi_ooc
    + alpha_phi + offset;
    
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu_s), exp(log_phi_s));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    sim_s_count_C[g] <- multinomial_rng(p, sum(s_count_C[g]));
    
    theta_C[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
    
    // Simulate infection prevelance measurement
    sim_prev_C[g] <- binomial_rng(N_mice, theta_C[g]);
    
  }
  
  for (g in 1:N_Tatv) {
    real log_mu_ooc;
    real log_phi_ooc;
    real log_mu_s;
    real log_phi_s;
    
    // Simulate oocyst count measurement
    log_mu_ooc <-  rho__log_mean_ooc_Tatv
    + tau_bite__log_mean_ooc_Tatv
    * rho_bite__log_mean_ooc_Tatv[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_Tatv
    * rho_round__log_mean_ooc_Tatv[(g - 1) % N_round + 1];
    log_phi_ooc <-  rho__log_od_ooc_Tatv
    + tau_bite__log_od_ooc_Tatv
    * rho_bite__log_od_ooc_Tatv[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_Tatv
    * rho_round__log_od_ooc_Tatv[(g - 1) % N_round + 1];
    
    sim_ooc_count_Tatv[g]
    <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
    
    theta_Tatv_mosquito[g] <- 1.0 - neg_binomial_2_cdf(0, exp(log_mu_ooc), exp(log_phi_ooc));
    
    // Simulate sporozoite count measurement
    log_mu_s <-  beta_mu[1] * log_mu_ooc
    + beta_mu[2] * log_phi_ooc
    + alpha_mu + offset;
    log_phi_s <-  beta_phi[1] * log_mu_ooc
    + beta_phi[2] * log_phi_ooc
    + alpha_phi + offset;
    
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu_s), exp(log_phi_s));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    sim_s_count_Tatv[g] <- multinomial_rng(p, sum(s_count_Tatv[g]));
    
    theta_Tatv[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
    
    // Simulate infection prevelance measurement
    sim_prev_Tatv[g] <- binomial_rng(N_mice, theta_Tatv[g]);
    
  }
 
  for (g in 1:N_T4b7) {
    real log_mu_ooc;
    real log_phi_ooc;
    real log_mu_s;
    real log_phi_s;
    
    // Simulate oocyst count measurement
    log_mu_ooc <-  rho__log_mean_ooc_T4b7
    + tau_bite__log_mean_ooc_T4b7
    * rho_bite__log_mean_ooc_T4b7[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_T4b7
    * rho_round__log_mean_ooc_T4b7[(g - 1) % N_round + 1];
    log_phi_ooc <-  rho__log_od_ooc_T4b7
    + tau_bite__log_od_ooc_T4b7
    * rho_bite__log_od_ooc_T4b7[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_T4b7
    * rho_round__log_od_ooc_T4b7[(g - 1) % N_round + 1];
    
    sim_ooc_count_T4b7[g]
    <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
    
    theta_T4b7_mosquito[g] <- 1.0 - neg_binomial_2_cdf(0, exp(log_mu_ooc), exp(log_phi_ooc));
    
    // Simulate sporozoite count measurement
    log_mu_s <-  beta_mu[1] * log_mu_ooc
    + beta_mu[2] * log_phi_ooc
    + alpha_mu + offset;
    log_phi_s <-  beta_phi[1] * log_mu_ooc
    + beta_phi[2] * log_phi_ooc
    + alpha_phi + offset;
    
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu_s), exp(log_phi_s));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    sim_s_count_T4b7[g] <- multinomial_rng(p, sum(s_count_T4b7[g]));
    
    theta_T4b7[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
    
    // Simulate infection prevelance measurement
    sim_prev_T4b7[g] <- binomial_rng(N_mice, theta_T4b7[g]);
    
  }

  for (g in 1:N_Tvacc) {
    real log_mu_ooc;
    real log_phi_ooc;
    real log_mu_s;
    real log_phi_s;
    
    // Simulate oocyst count measurement
    log_mu_ooc <-  rho__log_mean_ooc_Tvacc
    + tau_bite__log_mean_ooc_Tvacc
    * rho_bite__log_mean_ooc_Tvacc[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_Tvacc
    * rho_round__log_mean_ooc_Tvacc[(g - 1) % N_round + 1];
    log_phi_ooc <-  rho__log_od_ooc_Tvacc
    + tau_bite__log_od_ooc_Tvacc
    * rho_bite__log_od_ooc_Tvacc[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_Tvacc
    * rho_round__log_od_ooc_Tvacc[(g - 1) % N_round + 1];
    
    sim_ooc_count_Tvacc[g]
    <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
    
    theta_Tvacc_mosquito[g] <- 1.0 - neg_binomial_2_cdf(0, exp(log_mu_ooc), exp(log_phi_ooc));
    
    // Simulate sporozoite count measurement
    log_mu_s <-  beta_mu[1] * log_mu_ooc
    + beta_mu[2] * log_phi_ooc
    + alpha_mu + offset;
    log_phi_s <-  beta_phi[1] * log_mu_ooc
    + beta_phi[2] * log_phi_ooc
    + alpha_phi + offset;
    
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu_s), exp(log_phi_s));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    sim_s_count_Tvacc[g] <- multinomial_rng(p, sum(s_count_Tvacc[g]));
    
    theta_Tvacc[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
    
    // Simulate infection prevelance measurement
    sim_prev_Tvacc[g] <- binomial_rng(N_mice, theta_Tvacc[g]);
    
  }

  for (g in 1:N_Tatv_vacc) {
    real log_mu_ooc;
    real log_phi_ooc;
    real log_mu_s;
    real log_phi_s;
    
    // Simulate oocyst count measurement
    log_mu_ooc <-  rho__log_mean_ooc_Tatv_vacc
    + tau_bite__log_mean_ooc_Tatv_vacc
    * rho_bite__log_mean_ooc_Tatv_vacc[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_Tatv_vacc
    * rho_round__log_mean_ooc_Tatv_vacc[(g - 1) % N_round + 1];
    log_phi_ooc <-  rho__log_od_ooc_Tatv_vacc
    + tau_bite__log_od_ooc_Tatv_vacc
    * rho_bite__log_od_ooc_Tatv_vacc[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_Tatv_vacc
    * rho_round__log_od_ooc_Tatv_vacc[(g - 1) % N_round + 1];
    
    sim_ooc_count_Tatv_vacc[g]
    <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
    
    theta_Tatv_vacc_mosquito[g] <- 1.0 - neg_binomial_2_cdf(0, exp(log_mu_ooc), exp(log_phi_ooc));
    
    // Simulate sporozoite count measurement
    log_mu_s <-  beta_mu[1] * log_mu_ooc
    + beta_mu[2] * log_phi_ooc
    + alpha_mu + offset;
    log_phi_s <-  beta_phi[1] * log_mu_ooc
    + beta_phi[2] * log_phi_ooc
    + alpha_phi + offset;
    
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu_s), exp(log_phi_s));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    sim_s_count_Tatv_vacc[g] <- multinomial_rng(p, sum(s_count_Tatv_vacc[g]));
    
    theta_Tatv_vacc[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
    
    // Simulate infection prevelance measurement
    sim_prev_Tatv_vacc[g] <- binomial_rng(N_mice, theta_Tatv_vacc[g]);
    
  }

  for (g in 1:N_T4b7_vacc) {
    real log_mu_ooc;
    real log_phi_ooc;
    real log_mu_s;
    real log_phi_s;
    
    // Simulate oocyst count measurement
    log_mu_ooc <-  rho__log_mean_ooc_T4b7_vacc
    + tau_bite__log_mean_ooc_T4b7_vacc
    * rho_bite__log_mean_ooc_T4b7_vacc[(g - 1) / N_round + 1]
    + tau_round__log_mean_ooc_T4b7_vacc
    * rho_round__log_mean_ooc_T4b7_vacc[(g - 1) % N_round + 1];
    log_phi_ooc <-  rho__log_od_ooc_T4b7_vacc
    + tau_bite__log_od_ooc_T4b7_vacc
    * rho_bite__log_od_ooc_T4b7_vacc[(g - 1) / N_round + 1]
    + tau_round__log_od_ooc_T4b7_vacc
    * rho_round__log_od_ooc_T4b7_vacc[(g - 1) % N_round + 1];
    
    sim_ooc_count_T4b7_vacc[g]
    <- neg_binomial_2_log_rng(log_mu_ooc, exp(log_phi_ooc));
    
    theta_T4b7_vacc_mosquito[g] <- 1.0 - neg_binomial_2_cdf(0, exp(log_mu_ooc), exp(log_phi_ooc));
    
    // Simulate sporozoite count measurement
    log_mu_s <-  beta_mu[1] * log_mu_ooc
    + beta_mu[2] * log_phi_ooc
    + alpha_mu + offset;
    log_phi_s <-  beta_phi[1] * log_mu_ooc
    + beta_phi[2] * log_phi_ooc
    + alpha_phi + offset;
    
    for(b in 1:(N_bin - 1))
      cdf[b] <- neg_binomial_2_cdf(bin_edge[b], exp(log_mu_s), exp(log_phi_s));
    
    p[1] <- cdf[1];
    sum_p <- p[1];
    
    for(b in 2:(N_bin - 1)) {
      p[b] <- cdf[b] - cdf[b - 1];
      sum_p <- sum_p + p[b];
    }
    p[N_bin] <- 1 - sum_p;
    
    sim_s_count_T4b7_vacc[g] <- multinomial_rng(p, sum(s_count_T4b7_vacc[g]));
    
    theta_T4b7_vacc[g] <- inv_logit(  beta_theta[1] * log_mu_s
                              + beta_theta[2] * log_phi_s
                              + alpha_theta);
    
    // Simulate infection prevelance measurement
    sim_prev_T4b7_vacc[g] <- binomial_rng(N_mice, theta_T4b7_vacc[g]);
    
  }

}

}