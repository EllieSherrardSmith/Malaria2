data {
  // Number of control rounds/bites = 12 rounds 1 to 3 and bites 1, 2, 5, 10
  // Round 4 data being retaken
  int<lower=0> N_C;

  // Number of ATV25 treatment rounds (12) round 1 to 3 and bites 1, 2, 5, 10
  // In future data later rounds were not collected because the disease
  // was eradicated by the first round
  int<lower=0> N_T;

  // Number of oocysts in each round (45 subsampled from the full data)
  int<lower=0> N_ooc;

  // Number of mice in each trial
  int<lower=0> max_mice_C;    // 30
  int<lower=0> N_mice_C[N_C]; // Aggregated controls data
  int<lower=0> N_mice_T;      // Always 5

  // Raw data counts of the oocysts for each control trial
  int<lower=0> ooc_count_C[N_ooc, N_C];

  // Raw data counts of the oocysts for each ATV treatment trial
  int<lower=0> ooc_count_T[N_ooc, N_T];

  // Parasitemia count in each mouse in each control trial
  //int<lower=0> parasitemia_C[N_C, max_mice_C];
  //int<lower=0> parasitemia_T[N_C, N_mice_T];

  int<lower=0> prev_C[N_C, max_mice_C];
  int<lower=0> prev_T[N_T, N_mice_T];

  // Sporozoite count binning
  int<lower=0> N_bin;
  int<lower=0> bin_edge[N_bin + 1];

  // Binned sporozoite counts for each control/treatment group
  int<lower=0> sporo_count_C[N_C, N_bin];
  int<lower=0> sporo_count_T[N_T, N_bin];
}

transformed data {
  int N_bite;
  int N_round;
  real offset;

  N_bite <- 4;
  N_round <- 3;
  offset <- 4;
}

parameters {
  // Control
  real rho__log_ooc_mean_C;                 // Overall oocyst mean
  real<lower=0> tau_bite__log_ooc_mean_C;   // Bite number effect variation
  real rho_bite__log_ooc_mean_C[N_bite];    // Bite number effect
  real<lower=0> tau_round__log_ooc_mean_C;  // Round effect variation
  real rho_round__log_ooc_mean_C[N_round];  // Round effect

  real rho__log_ooc_agg_C;                   // Overall oocyst aggregation
  real<lower=0> tau_bite__log_ooc_agg_C;     // Bite number effect variation
  real rho_bite__log_ooc_agg_C[N_bite];      // Bite number effect
  real<lower=0> tau_round__log_ooc_agg_C;    // Round effect variation
  real rho_round__log_ooc_agg_C[N_round];    // Round effect

  // Treatment
  real rho__log_ooc_mean_T;                  // Overall oocyst mean
  real<lower=0> tau_bite__log_ooc_mean_T;    // Bite number effect variation
  real rho_bite__log_ooc_mean_T[N_bite];     // Bite number effect
  real<lower=0> tau_round__log_ooc_mean_T;   // Round effect variation
  real rho_round__log_ooc_mean_T[N_round];   // Round effect

  real rho__log_ooc_agg_T;                   // Overall oocyst aggregation
  real<lower=0> tau_bite__log_ooc_agg_T;     // Bite number effect variation
  real rho_bite__log_ooc_agg_T[N_bite];      // Bite number effect
  real<lower=0> tau_round__log_ooc_agg_T;    // Round effect variation
  real rho_round__log_ooc_agg_T[N_round];    // Round effect

  // Slopes and intercepts of GLM modeling conversion from oocyst counts
  // to sporozoite counts.  Common slopes for control and treatment groups
  // as the underlying process should be independent of treatment.
  vector[2] beta_ooc_to_sporo_mean;
  vector[2] beta_ooc_to_sporo_agg;
  real alpha_ooc_to_sporo_mean;
  real alpha_ooc_to_sporo_agg;

  // Slopes and intercept of GLM modeling infection probability from
  // sporozite count distribution.  Again, common parameters for control
  // and treatment as the underlying process should be independent of
  // treatment.
  vector[2] beta_theta;
  real alpha_theta;

  /*
  // Slopes and intercept of GLM modeling conversion from sporozite counts
  // to parasitemia counts.  Conversion should depend on the number of
  // bites as more bites mean high susceptability
  real rho__beta_sporo_mean_to_paras_mean;
  real<lower=0> tau_bite__beta_sporo_mean_to_paras_mean;
  real rho_bite__beta_sporo_mean_to_paras_mean[N_bite];

  real rho__beta_sporo_agg_to_paras_mean;
  real<lower=0> tau_bite__beta_sporo_agg_to_paras_mean;
  real rho_bite__beta_sporo_agg_to_paras_mean[N_bite];

  real rho__alpha_paras_mean;
  real<lower=0> tau_bite__alpha_paras_mean;
  real rho_bite__alpha_paras_mean[N_bite];

  real rho__beta_sporo_mean_to_paras_agg;
  real<lower=0> tau_bite__beta_sporo_mean_to_paras_agg;
  real rho_bite__beta_sporo_mean_to_paras_agg[N_bite];

  real rho__beta_sporo_agg_to_paras_agg;
  real<lower=0> tau_bite__beta_sporo_agg_to_paras_agg;
  real rho_bite__beta_sporo_agg_to_paras_agg[N_bite];

  real rho__alpha_paras_agg;
  real<lower=0> tau_bite__alpha_paras_agg;
  real rho_bite__alpha_paras_agg[N_bite];
  */
}

model {

  // Log mean and log aggregation of control group oocyst count NegBin distribution
  vector[N_C] log_ooc_mean_C;
  vector[N_C] log_ooc_agg_C;

  // Log mean and log aggregation of treatment group oocyst count NegBin distribution
  vector[N_T] log_ooc_mean_T;
  vector[N_T] log_ooc_agg_T;

  vector[N_C] logit_theta_C;
  vector[N_T] logit_theta_T;

  // Sporozoite count censoring model parameters
  real sum_p;
  vector[N_bin] p;
  vector[N_bin - 1] cdf;

  // Priors
  rho__log_ooc_mean_C ~ normal(0, 5);
  tau_bite__log_ooc_mean_C ~ cauchy(0, 2.5);
  rho_bite__log_ooc_mean_C ~ normal(0, 1);
  tau_round__log_ooc_mean_C ~ cauchy(0, 2.5);
  rho_round__log_ooc_mean_C ~ normal(0, 1);

  rho__log_ooc_agg_C ~ normal(0, 5);
  tau_bite__log_ooc_agg_C ~ cauchy(0, 2.5);
  rho_bite__log_ooc_agg_C ~ normal(0, 1);
  tau_round__log_ooc_agg_C ~ cauchy(0, 2.5);
  rho_round__log_ooc_agg_C ~ normal(0, 1);

  rho__log_ooc_mean_T ~ normal(0, 5);
  tau_bite__log_ooc_mean_T ~ cauchy(0, 2.5);
  rho_bite__log_ooc_mean_T ~ normal(0, 1);
  tau_round__log_ooc_mean_T ~ cauchy(0, 2.5);
  rho_round__log_ooc_mean_T ~ normal(0, 1);

  rho__log_ooc_agg_T ~ normal(0, 5);
  tau_bite__log_ooc_agg_T ~ cauchy(0, 2.5);
  rho_bite__log_ooc_agg_T ~ normal(0, 1);
  tau_round__log_ooc_agg_T ~ cauchy(0, 2.5);
  rho_round__log_ooc_agg_T ~ normal(0, 1);

  beta_ooc_to_sporo_mean ~ normal(0, 8);
  beta_ooc_to_sporo_agg ~ normal(0, 8);
  alpha_ooc_to_sporo_mean ~ normal(0, 8);
  alpha_ooc_to_sporo_agg ~ normal(0, 8);

  beta_theta ~ normal(0, 4);
  alpha_theta ~ normal(0, 4);

  /*
  rho__beta_sporo_mean_to_paras_mean ~ normal(0, 4);
  tau_bite__beta_sporo_mean_to_paras_mean ~ cauchy(0, 2.5);
  rho_bite__beta_sporo_mean_to_paras_mean ~ normal(0, 1);

  rho__beta_sporo_agg_to_paras_mean ~ normal(0, 4);
  tau_bite__beta_sporo_agg_to_paras_mean ~ cauchy(0, 2.5);
  rho_bite__beta_sporo_agg_to_paras_mean ~ normal(0, 1);

  rho__alpha_paras_mean ~ normal(0, 4);
  tau_bite__alpha_paras_mean ~ cauchy(0, 2.5);
  rho_bite__alpha_paras_mean ~ normal(0, 1);

  rho__beta_sporo_mean_to_paras_agg ~ normal(0, 4);
  tau_bite__beta_sporo_mean_to_paras_agg ~ cauchy(0, 2.5);
  rho_bite__beta_sporo_mean_to_paras_agg ~ normal(0, 1);

  rho__beta_sporo_agg_to_paras_agg ~ normal(0, 4);
  tau_bite__beta_sporo_agg_to_paras_agg ~ cauchy(0, 2.5);
  rho_bite__beta_sporo_agg_to_paras_agg ~ normal(0, 1);

  rho__alpha_paras_agg ~ normal(0, 4);
  tau_bite__alpha_paras_agg ~ cauchy(0, 2.5);
  rho_bite__alpha_paras_agg ~ normal(0, 1);
  */
  // Oocyst measureuments (control)
  for (n in 1:N_C) {
    log_ooc_mean_C[n] <-   rho__log_ooc_mean_C
                           + tau_bite__log_ooc_mean_C
                             * rho_bite__log_ooc_mean_C[(n - 1) / N_round + 1]
                           + tau_round__log_ooc_mean_C
                             * rho_round__log_ooc_mean_C[(n - 1) % N_round + 1];
    log_ooc_agg_C[n] <-  rho__log_ooc_agg_C
                         + tau_bite__log_ooc_agg_C
                           * rho_bite__log_ooc_agg_C[(n - 1) / N_round + 1]
                         + tau_round__log_ooc_agg_C
                           * rho_round__log_ooc_agg_C[(n - 1) % N_round + 1];
  }

  for(n in 1:N_ooc)
    ooc_count_C[n] ~ neg_binomial_2_log(log_ooc_mean_C, exp(log_ooc_agg_C));

  // Oocyst measureuments (treatment)
  for (n in 1:N_T) {
    log_ooc_mean_T[n] <-   rho__log_ooc_mean_T
                           + tau_bite__log_ooc_mean_T
                             * rho_bite__log_ooc_mean_T[(n - 1) / N_round + 1]
                           + tau_round__log_ooc_mean_T
                             * rho_round__log_ooc_mean_T[(n - 1) % N_round + 1];
    log_ooc_agg_T[n] <-  rho__log_ooc_agg_T
                       + tau_bite__log_ooc_agg_T
                         * rho_bite__log_ooc_agg_T[(n - 1) / N_round + 1]
                       + tau_round__log_ooc_agg_T
                         * rho_round__log_ooc_agg_T[(n - 1) % N_round + 1];
  }

  for(n in 1:N_ooc)
    ooc_count_T[n] ~ neg_binomial_2_log(log_ooc_mean_T, exp(log_ooc_agg_T));

  // Censored sporozoite measurements (control)
  for (n in 1:N_C) {
    real log_sporo_mean;
    real log_sporo_agg;

    real beta_sporo_mean_to_paras_mean;
    real beta_sporo_agg_to_paras_mean;
    real alpha_paras_mean;

    real beta_sporo_mean_to_paras_agg;
    real beta_sporo_agg_to_paras_agg;
    real alpha_paras_agg;

    real log_paras_mean;
    real log_paras_agg;

    // GLM of sporozoite count distribution in place of intractable convolution
    log_sporo_mean <-  beta_ooc_to_sporo_mean[1] * log_ooc_mean_C[n]
                     + beta_ooc_to_sporo_mean[2] * log_ooc_agg_C[n]
                     + alpha_ooc_to_sporo_mean + offset;
    log_sporo_agg <-  beta_ooc_to_sporo_agg[1] * log_ooc_mean_C[n]
                    + beta_ooc_to_sporo_agg[2] * log_ooc_agg_C[n]
                    + alpha_ooc_to_sporo_agg + offset;

    // Compute censored multinomial probabilities
    for(m in 1:(N_bin - 1))
      cdf[m] <- neg_binomial_2_cdf(bin_edge[m], exp(log_sporo_mean), exp(log_sporo_agg));

    p[1] <- cdf[1];
    sum_p <- p[1];

    for(m in 2:(N_bin - 1)) {
      p[m] <- cdf[m] - cdf[m - 1];
      sum_p <- sum_p + p[m];
    }
    p[N_bin] <- 1 - sum_p;

    // Censored measurement
    sporo_count_C[n] ~ multinomial(p);

    // GLM of infection probability in place of intractable convolution
    logit_theta_C[n] <-    beta_theta[1] * log_sporo_mean
                         + beta_theta[2] * log_sporo_agg
                         + alpha_theta;

    for (m in 1:N_mice_C[n])
      prev_C[n, m] ~ bernoulli_logit(logit_theta_C[n]);

    /*
    // GLM of parasitemia percentage in place of intractable convolution
    beta_sporo_mean_to_paras_mean <-  rho__beta_sporo_mean_to_paras_mean
                                    + tau_bite__beta_sporo_mean_to_paras_mean
                                      * rho_bite__beta_sporo_mean_to_paras_mean[(n - 1) / N_round + 1];

    beta_sporo_agg_to_paras_mean <-  rho__beta_sporo_agg_to_paras_mean
                                    + tau_bite__beta_sporo_agg_to_paras_mean
                                      * rho_bite__beta_sporo_agg_to_paras_mean[(n - 1) / N_round + 1];

    alpha_paras_mean <-  rho__alpha_paras_mean
                       + tau_bite__alpha_paras_mean
                         * rho_bite__alpha_paras_mean[(n - 1) / N_round + 1];

    log_paras_mean <-  beta_sporo_mean_to_paras_mean * log_sporo_mean
                     + beta_sporo_agg_to_paras_mean * log_sporo_agg
                     + alpha_paras_mean;

    beta_sporo_mean_to_paras_agg <-  rho__beta_sporo_mean_to_paras_agg
                                   + tau_bite__beta_sporo_mean_to_paras_agg
                                     * rho_bite__beta_sporo_mean_to_paras_agg[(n - 1) / N_round + 1];

    beta_sporo_agg_to_paras_agg <-  rho__beta_sporo_agg_to_paras_agg
                                   + tau_bite__beta_sporo_agg_to_paras_agg
                                     * rho_bite__beta_sporo_agg_to_paras_agg[(n - 1) / N_round + 1];

    alpha_paras_agg <-  rho__alpha_paras_agg
                      + tau_bite__alpha_paras_agg
                        * rho_bite__alpha_paras_agg[(n - 1) / N_round + 1];

    log_paras_agg <-  beta_sporo_mean_to_paras_agg * log_sporo_mean
                    + beta_sporo_agg_to_paras_agg * log_sporo_agg
                    + alpha_paras_agg;

    for (m in 1:N_mice_C[n])
      parasitemia_C[n, m] ~ neg_binomial_2_log(log_paras_mean, exp(log_paras_agg));
    */
  }

  // Censored sporozoite measurements (ATV treatment)
  // Treatment group distributions
  for (n in 1:N_T) {
    real log_sporo_mean;
    real log_sporo_agg;

    real beta_sporo_mean_to_paras_mean;
    real beta_sporo_agg_to_paras_mean;
    real alpha_paras_mean;

    real beta_sporo_mean_to_paras_agg;
    real beta_sporo_agg_to_paras_agg;
    real alpha_paras_agg;

    real log_paras_mean;
    real log_paras_agg;

    // GLM of sporozoite count distribution in place of intractable convolution
    log_sporo_mean <-  beta_ooc_to_sporo_mean[1] * log_ooc_mean_T[n]
                     + beta_ooc_to_sporo_mean[2] * log_ooc_agg_T[n]
                     + alpha_ooc_to_sporo_mean + offset;
    log_sporo_agg <-  beta_ooc_to_sporo_agg[1] * log_ooc_mean_T[n]
                    + beta_ooc_to_sporo_agg[2] * log_ooc_agg_T[n]
                    + alpha_ooc_to_sporo_agg + offset;

    // Compute censored multinomial probabilities
    for(m in 1:(N_bin - 1))
      cdf[m] <- neg_binomial_2_cdf(bin_edge[m], exp(log_sporo_mean), exp(log_sporo_agg));

    p[1] <- cdf[1];
    sum_p <- p[1];

    for(m in 2:(N_bin - 1)) {
      p[m] <- cdf[m] - cdf[m - 1];
      sum_p <- sum_p + p[m];
    }
    p[N_bin] <- 1 - sum_p;

    // Censored measurement
    sporo_count_T[n] ~ multinomial(p);

    // GLM of infection probability in place of intractable convolution
    logit_theta_T[n] <-    beta_theta[1] * log_sporo_mean
                         + beta_theta[2] * log_sporo_agg
                         + alpha_theta;

    for (m in 1:N_mice_T)
      prev_T[n, m] ~ bernoulli_logit(logit_theta_T[n]);

   /*
    // GLM of parasitemia percentage in place of intractable convolution
    beta_sporo_mean_to_paras_mean <-  rho__beta_sporo_mean_to_paras_mean
                                    + tau_bite__beta_sporo_mean_to_paras_mean
                                      * rho_bite__beta_sporo_mean_to_paras_mean[(n - 1) / N_round + 1];

    beta_sporo_agg_to_paras_mean <-  rho__beta_sporo_agg_to_paras_mean
                                    + tau_bite__beta_sporo_agg_to_paras_mean
                                      * rho_bite__beta_sporo_agg_to_paras_mean[(n - 1) / N_round + 1];

    alpha_paras_mean <-  rho__alpha_paras_mean
                       + tau_bite__alpha_paras_mean
                         * rho_bite__alpha_paras_mean[(n - 1) / N_round + 1];

    log_paras_mean <-  beta_sporo_mean_to_paras_mean * log_sporo_mean
                     + beta_sporo_agg_to_paras_mean * log_sporo_agg
                     + alpha_paras_mean;

    beta_sporo_mean_to_paras_agg <-  rho__beta_sporo_mean_to_paras_agg
                                  + tau_bite__beta_sporo_mean_to_paras_agg
                                    * rho_bite__beta_sporo_mean_to_paras_agg[(n - 1) / N_round + 1];

    beta_sporo_agg_to_paras_agg <-  rho__beta_sporo_agg_to_paras_agg
                                  + tau_bite__beta_sporo_agg_to_paras_agg
                                    * rho_bite__beta_sporo_agg_to_paras_agg[(n - 1) / N_round + 1];

    alpha_paras_agg <-  rho__alpha_paras_agg
                     + tau_bite__alpha_paras_agg
                       * rho_bite__alpha_paras_agg[(n - 1) / N_round + 1];

    log_paras_agg <-  beta_sporo_mean_to_paras_agg * log_sporo_mean
                   + beta_sporo_agg_to_paras_agg * log_sporo_agg
                   + alpha_paras_agg;

    for (m in 1:N_mice_C[n])
     parasitemia_C[n, m] ~ neg_binomial_2_log(log_paras_mean, exp(log_paras_agg));
    */
  }

}

generated quantities {
  int<lower=0> sim_ooc_count_C[N_C];
  int<lower=0> sim_ooc_count_T[N_T];

  int<lower=0> sim_sporo_count_C[N_C, N_bin];
  int<lower=0> sim_sporo_count_T[N_T, N_bin];

  real<lower=0, upper=1> theta_C[N_C];
  real<lower=0, upper=1> theta_T[N_T];

  real<lower=0, upper=1> theta_C_mosquito[N_C];
  real<lower=0, upper=1> theta_T_mosquito[N_T];

  int<lower=0, upper=max_mice_C> sim_prev_C[N_C];
  int<lower=0, upper=N_mice_T> sim_prev_T[ N_T];

  //int sim_parasitemia_C[N_C, max_mice_C];
  //int sim_parasitemia_T[N_C, N_mice_T];

  {
    real sum_p;
    vector[N_bin] p;
    vector[N_bin - 1] cdf;

    for (n in 1:N_C) {
      real log_ooc_mean;
      real log_ooc_agg;
      real log_sporo_mean;
      real log_sporo_agg;

      real beta_sporo_mean_to_paras_mean;
      real beta_sporo_agg_to_paras_mean;
      real alpha_paras_mean;

      real beta_sporo_mean_to_paras_agg;
      real beta_sporo_agg_to_paras_agg;
      real alpha_paras_agg;

      real log_paras_mean;
      real log_paras_agg;

      // Simulate oocyst count measurement
      log_ooc_mean <-  rho__log_ooc_mean_C
                     + tau_bite__log_ooc_mean_C
                       * rho_bite__log_ooc_mean_C[(n - 1) / N_round + 1]
                     + tau_round__log_ooc_mean_C
                       * rho_round__log_ooc_mean_C[(n - 1) % N_round + 1];
      log_ooc_agg <-  rho__log_ooc_agg_C
                    + tau_bite__log_ooc_agg_C
                      * rho_bite__log_ooc_agg_C[(n - 1) / N_round + 1]
                    + tau_round__log_ooc_agg_C
                      * rho_round__log_ooc_agg_C[(n - 1) % N_round + 1];

      sim_ooc_count_C[n]
        <- neg_binomial_2_log_rng(log_ooc_mean, exp(log_ooc_agg));

      theta_C_mosquito[n] <- 1.0 - neg_binomial_2_cdf(0, exp(log_ooc_mean), exp(log_ooc_agg));

      // Simulate sporozoite count measurement
      log_sporo_mean <-  beta_ooc_to_sporo_mean[1] * log_ooc_mean
                       + beta_ooc_to_sporo_mean[2] * log_ooc_agg
                       + alpha_ooc_to_sporo_mean + offset;
      log_sporo_agg <-  beta_ooc_to_sporo_agg[1] * log_ooc_mean
                      + beta_ooc_to_sporo_agg[2] * log_ooc_agg
                      + alpha_ooc_to_sporo_agg + offset;

      for(m in 1:(N_bin - 1))
        cdf[m] <- neg_binomial_2_cdf(bin_edge[m], exp(log_sporo_mean), exp(log_sporo_agg));

      p[1] <- cdf[1];
      sum_p <- p[1];

      for(m in 2:(N_bin - 1)) {
        p[m] <- cdf[m] - cdf[m - 1];
        sum_p <- sum_p + p[m];
      }
      p[N_bin] <- 1 - sum_p;

      sim_sporo_count_C[n] <- multinomial_rng(p, sum(sporo_count_C[n]));

      theta_C[n] <- inv_logit(  beta_theta[1] * log_sporo_mean
                              + beta_theta[2] * log_sporo_agg
                              + alpha_theta);

      sim_prev_C[n] <- binomial_rng(N_mice_C[n], theta_C[n]);

      /*
      // Simulate parasitemia measurement
      beta_sporo_mean_to_paras_mean <-  rho__beta_sporo_mean_to_paras_mean
                                      + tau_bite__beta_sporo_mean_to_paras_mean
                                        * rho_bite__beta_sporo_mean_to_paras_mean[(n - 1) / N_round + 1];

      beta_sporo_agg_to_paras_mean <-  rho__beta_sporo_agg_to_paras_mean
                                      + tau_bite__beta_sporo_agg_to_paras_mean
                                        * rho_bite__beta_sporo_agg_to_paras_mean[(n - 1) / N_round + 1];

      alpha_paras_mean <-  rho__alpha_paras_mean
                         + tau_bite__alpha_paras_mean
                           * rho_bite__alpha_paras_mean[(n - 1) / N_round + 1];

      log_paras_mean <-  beta_sporo_mean_to_paras_mean * log_sporo_mean
                       + beta_sporo_agg_to_paras_mean * log_sporo_agg
                       + alpha_paras_mean;

      beta_sporo_mean_to_paras_agg <-  rho__beta_sporo_mean_to_paras_agg
                                     + tau_bite__beta_sporo_mean_to_paras_agg
                                       * rho_bite__beta_sporo_mean_to_paras_agg[(n - 1) / N_round + 1];

      beta_sporo_agg_to_paras_agg <-  rho__beta_sporo_agg_to_paras_agg
                                     + tau_bite__beta_sporo_agg_to_paras_agg
                                       * rho_bite__beta_sporo_agg_to_paras_agg[(n - 1) / N_round + 1];

      alpha_paras_agg <-  rho__alpha_paras_agg
                        + tau_bite__alpha_paras_agg
                          * rho_bite__alpha_paras_agg[(n - 1) / N_round + 1];

      log_paras_agg <-  beta_sporo_mean_to_paras_agg * log_sporo_mean
                      + beta_sporo_agg_to_paras_agg * log_sporo_agg
                      + alpha_paras_agg;

      for (m in 1:N_mice_C[n])
       sim_parasitemia_C[n, m] <- neg_binomial_2_log_rng(log_paras_mean, exp(log_paras_agg));
       */
    }

    for (n in 1:N_T) {
      real log_ooc_mean;
      real log_ooc_agg;
      real log_sporo_mean;
      real log_sporo_agg;

      real beta_sporo_mean_to_paras_mean;
      real beta_sporo_agg_to_paras_mean;
      real alpha_paras_mean;

      real beta_sporo_mean_to_paras_agg;
      real beta_sporo_agg_to_paras_agg;
      real alpha_paras_agg;

      real log_paras_mean;
      real log_paras_agg;

      // Simulate oocyst count measurement
      log_ooc_mean <-  rho__log_ooc_mean_T
                     + tau_bite__log_ooc_mean_T
                       * rho_bite__log_ooc_mean_T[(n - 1) / N_round + 1]
                     + tau_round__log_ooc_mean_T
                       * rho_round__log_ooc_mean_T[(n - 1) % N_round + 1];
      log_ooc_agg <-  rho__log_ooc_agg_T
                    + tau_bite__log_ooc_agg_T
                      * rho_bite__log_ooc_agg_T[(n - 1) / N_round + 1]
                    + tau_round__log_ooc_agg_T
                      * rho_round__log_ooc_agg_T[(n - 1) % N_round + 1];

      sim_ooc_count_T[n]
        <- neg_binomial_2_log_rng(log_ooc_mean, exp(log_ooc_agg));

      theta_T_mosquito[n] <- 1.0 - neg_binomial_2_cdf(0, exp(log_ooc_mean), exp(log_ooc_agg));

      // Simulate sporozoite count measurement
      log_sporo_mean <-  beta_ooc_to_sporo_mean[1] * log_ooc_mean
                       + beta_ooc_to_sporo_mean[2] * log_ooc_agg
                       + alpha_ooc_to_sporo_mean + offset;
      log_sporo_agg <-  beta_ooc_to_sporo_agg[1] * log_ooc_mean
                      + beta_ooc_to_sporo_agg[2] * log_ooc_agg
                      + alpha_ooc_to_sporo_agg + offset;

      for(m in 1:(N_bin - 1))
        cdf[m] <- neg_binomial_2_cdf(bin_edge[m], exp(log_sporo_mean), exp(log_sporo_agg));

      p[1] <- cdf[1];
      sum_p <- p[1];

      for(m in 2:(N_bin - 1)) {
        p[m] <- cdf[m] - cdf[m - 1];
        sum_p <- sum_p + p[m];
      }
      p[N_bin] <- 1 - sum_p;

      sim_sporo_count_T[n] <- multinomial_rng(p, sum(sporo_count_T[n]));

      theta_T[n] <- inv_logit(  beta_theta[1] * log_sporo_mean
                              + beta_theta[2] * log_sporo_agg
                              + alpha_theta);

      sim_prev_C[n] <- binomial_rng(N_mice_T, theta_T[n]);

      /*
      // Simulate parasitemia measurement
      beta_sporo_mean_to_paras_mean <-  rho__beta_sporo_mean_to_paras_mean
                                      + tau_bite__beta_sporo_mean_to_paras_mean
                                        * rho_bite__beta_sporo_mean_to_paras_mean[(n - 1) / N_round + 1];

      beta_sporo_agg_to_paras_mean <-  rho__beta_sporo_agg_to_paras_mean
                                      + tau_bite__beta_sporo_agg_to_paras_mean
                                        * rho_bite__beta_sporo_agg_to_paras_mean[(n - 1) / N_round + 1];

      alpha_paras_mean <-  rho__alpha_paras_mean
                         + tau_bite__alpha_paras_mean
                           * rho_bite__alpha_paras_mean[(n - 1) / N_round + 1];

      log_paras_mean <-  beta_sporo_mean_to_paras_mean * log_sporo_mean
                       + beta_sporo_agg_to_paras_mean * log_sporo_agg
                       + alpha_paras_mean;

      beta_sporo_mean_to_paras_agg <-  rho__beta_sporo_mean_to_paras_agg
                                     + tau_bite__beta_sporo_mean_to_paras_agg
                                       * rho_bite__beta_sporo_mean_to_paras_agg[(n - 1) / N_round + 1];

      beta_sporo_agg_to_paras_agg <-  rho__beta_sporo_agg_to_paras_agg
                                     + tau_bite__beta_sporo_agg_to_paras_agg
                                       * rho_bite__beta_sporo_agg_to_paras_agg[(n - 1) / N_round + 1];

      alpha_paras_agg <-  rho__alpha_paras_agg
                        + tau_bite__alpha_paras_agg
                          * rho_bite__alpha_paras_agg[(n - 1) / N_round + 1];

      log_paras_agg <-  beta_sporo_mean_to_paras_agg * log_sporo_mean
                      + beta_sporo_agg_to_paras_agg * log_sporo_agg
                      + alpha_paras_agg;

      for (m in 1:N_mice_C[n])
       sim_parasitemia_C[n, m] <- neg_binomial_2_log_rng(log_paras_mean, exp(log_paras_agg));
       */
    }

  }

}
