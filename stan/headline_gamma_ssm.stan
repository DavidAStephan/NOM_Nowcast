// Phase 3 v0.2 — Bayesian headline SSM with Gamma-lag visa observation.
//
// Architecture: damped local-linear trend on a single latent log-arrivals
// state mu_t; trivariate observations on log scale; visa-grants row
// loads on a log-sum-exp of forward-lagged mu's via fixed Gamma weights.
//
// Parameterisation: non-centred on the state innovations (mu_raw,
// beta_raw ~ std_normal). The joint posterior has a sigma_oad ↔
// sigma_mu identifiability funnel (correlation ~ -0.7) so HMC pays
// for it in treedepth saturation; but switching to a centred
// parameterisation breaks convergence on sigma_beta — so NCP it
// stays.  The driver script should run with adapt_delta >= 0.9 and
// iter_warmup >= 500 so the mass-matrix adaptation has enough room
// to find a good metric for the funnel.

data {
  int<lower=1> T;
  int<lower=1> T_obs;
  int<lower=1> n_w;
  array[T_obs] real y_oad;
  array[T_obs] int<lower=0, upper=1> has_oad;
  array[T_obs] real y_visa;
  array[T_obs] int<lower=0, upper=1> has_visa;
  array[T_obs] real y_nom;
  array[T_obs] int<lower=0, upper=1> has_nom;
  vector<lower=0, upper=1>[n_w] gamma_weights;
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
