// Phase 3 v0.3 — Bayesian headline SSM with *learned* Gamma lag.
//
// Generalises headline_gamma_ssm.stan by sampling the Gamma shape
// (alpha_gam) and rate (beta_gam) jointly with everything else.
// The Gamma weights w_k for k = 0..n_w-1 are recomputed every
// leapfrog step from the current draw of (alpha_gam, beta_gam) and
// normalised to sum to 1.
//
// Compared to the Kalman v5 grid search, HMC explores the (alpha,
// beta) space continuously and propagates uncertainty about the
// lag-shape into the posterior on nom_hat.
//
// Identifiability note: the visa observation only sees a weighted
// sum of future arrivals, so there's some degeneracy in (alpha,
// beta). Weakly informative priors centred on the Kalman v5 grid
// keep the sampler in the sensible region.

data {
  int<lower=1> T;
  int<lower=1> T_obs;
  int<lower=1> n_w;                        // n_w = K_max + 1
  array[T_obs] real y_oad;
  array[T_obs] int<lower=0, upper=1> has_oad;
  array[T_obs] real y_visa;
  array[T_obs] int<lower=0, upper=1> has_visa;
  array[T_obs] real y_nom;
  array[T_obs] int<lower=0, upper=1> has_nom;
  real mu0_loc;
  real<lower=0> mu0_scale;
  real<lower=0> slope0_scale;
  real<lower=0> sigma_obs_scale;
  real<lower=0> sigma_state_scale;
  real<lower=0> alpha_scale;
  real<lower=0, upper=1> phi_loc;
  real<lower=0> phi_conc;
  // Hyperpriors on the Gamma lag (shape, rate). Defaults give means
  // (alpha=4, beta=2) and 95% mass in the kalman v5 grid box.
  real<lower=0> alpha_gam_shape;
  real<lower=0> alpha_gam_rate;
  real<lower=0> beta_gam_shape;
  real<lower=0> beta_gam_rate;
  int<lower=0, upper=1> prior_only;
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
  // Learned Gamma-lag parameters
  real<lower=0.3, upper=10> alpha_gam;
  real<lower=0.1, upper=5>  beta_gam;
}

transformed parameters {
  vector[T] mu;
  vector[T] beta_slope;
  vector[n_w] gamma_weights;
  vector[n_w] log_w;

  mu[1]         = mu_init;
  beta_slope[1] = beta_init;
  for (t in 2:T) {
    mu[t]         = mu[t - 1] + beta_slope[t - 1] + sigma_mu   * mu_raw[t - 1];
    beta_slope[t] = phi * beta_slope[t - 1]      + sigma_beta * beta_raw[t - 1];
  }
  {
    vector[n_w] raw_w;
    for (k in 1:n_w) {
      real k_idx = k - 1;
      real lo = fmax(k_idx - 0.5, 1e-6);  // gamma_cdf(0 | .) is 0; nudge slightly off
      real hi = k_idx + 0.5;
      raw_w[k] = gamma_cdf(hi | alpha_gam, beta_gam)
               - gamma_cdf(lo | alpha_gam, beta_gam);
    }
    // Guard against the (rare) case where raw_w sums to numerically
    // zero (extreme tail draws); fall back to uniform.
    real raw_sum = sum(raw_w);
    if (raw_sum < 1e-12) {
      gamma_weights = rep_vector(1.0 / n_w, n_w);
    } else {
      gamma_weights = raw_w / raw_sum;
    }
    log_w = log(gamma_weights);
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
  alpha_gam ~ gamma(alpha_gam_shape, alpha_gam_rate);
  beta_gam  ~ gamma(beta_gam_shape,  beta_gam_rate);

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
