// Phase 3 v0.2 — Bayesian headline SSM with Gamma-lag visa observation.
//
// Same architecture as headline_ssm.stan plus a forward Gamma-lag
// on the visa-grants observation row, matching kalman_v4. Visa
// grants observed at quarter t load on a log-sum-exp of arrivals
// at quarters t+1..t+K_max with weights w_k from a Gamma(alpha,
// beta) pmf:
//
//   y_visa[t] = log(sum_k w_k * exp(mu[t + k])) + alpha_v + epsilon_v
//
// This propagates the visa signal forward into the latent state at
// future horizons — exactly what we want for the h=0 nowcast since
// NOM publishes with a ~6-month lag but visa grants are observed
// quarterly.
//
// Non-centered transitions, weakly informative priors.

data {
  int<lower=1> T;                          // total panel length (obs + h_max + K_max)
  int<lower=1> T_obs;                      // last observed period
  int<lower=1> n_w;                        // number of Gamma weights (k=0..K_max => n_w = K_max + 1)
  array[T_obs] real y_oad;
  array[T_obs] int<lower=0, upper=1> has_oad;
  array[T_obs] real y_visa;
  array[T_obs] int<lower=0, upper=1> has_visa;
  array[T_obs] real y_nom;
  array[T_obs] int<lower=0, upper=1> has_nom;
  vector<lower=0, upper=1>[n_w] gamma_weights;    // normalised pmf, supplied from R
  real mu0_loc;
  real<lower=0> mu0_scale;
  real<lower=0> slope0_scale;
  real<lower=0> sigma_obs_scale;
  real<lower=0> sigma_state_scale;
  real<lower=0> alpha_scale;
  real<lower=0, upper=1> phi_loc;
  real<lower=0> phi_conc;
  int<lower=0, upper=1> prior_only;
}

transformed data {
  vector[n_w] log_w = log(gamma_weights);
}

parameters {
  real mu_init;
  real beta_init;
  vector[T - 1] mu_raw;
  vector[T - 1] beta_raw;
  real alpha_v;
  real alpha_n;
  real<lower=0> sigma_oad;
  real<lower=0> sigma_visa;
  real<lower=0> sigma_nom;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_beta;
  real<lower=0, upper=1> phi;
}

transformed parameters {
  vector[T] mu;
  vector[T] beta_slope;
  mu[1]         = mu_init;
  beta_slope[1] = beta_init;
  for (t in 2:T) {
    mu[t]         = mu[t - 1] + beta_slope[t - 1] + sigma_mu   * mu_raw[t - 1];
    beta_slope[t] = phi * beta_slope[t - 1]      + sigma_beta * beta_raw[t - 1];
  }
}

model {
  mu_init    ~ normal(mu0_loc, mu0_scale);
  beta_init  ~ normal(0, slope0_scale);
  mu_raw     ~ std_normal();
  beta_raw   ~ std_normal();
  alpha_v    ~ normal(0, alpha_scale);
  alpha_n    ~ normal(0, alpha_scale);
  sigma_oad  ~ normal(0, sigma_obs_scale);
  sigma_visa ~ normal(0, sigma_obs_scale);
  sigma_nom  ~ normal(0, sigma_obs_scale);
  sigma_mu   ~ normal(0, sigma_state_scale);
  sigma_beta ~ normal(0, sigma_state_scale);
  phi ~ beta(phi_loc * phi_conc, (1 - phi_loc) * phi_conc);

  if (!prior_only) {
    for (t in 1:T_obs) {
      if (has_oad[t])  y_oad[t] ~ normal(mu[t],            sigma_oad);
      if (has_nom[t])  y_nom[t] ~ normal(mu[t] + alpha_n,  sigma_nom);
      if (has_visa[t] && t + n_w - 1 <= T) {
        // visa at t loads on mu[t + 0], mu[t + 1], ..., mu[t + n_w - 1]
        vector[n_w] terms;
        for (k in 1:n_w) terms[k] = log_w[k] + mu[t + k - 1];
        y_visa[t] ~ normal(log_sum_exp(terms) + alpha_v, sigma_visa);
      }
    }
  }
}

generated quantities {
  vector[T] log_nom_hat;
  vector[T] nom_hat;
  for (t in 1:T) {
    log_nom_hat[t] = mu[t] + alpha_n;
    nom_hat[t]     = exp(log_nom_hat[t]);
  }
}
