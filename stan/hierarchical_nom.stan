// Phase 3 — Bayesian hierarchical NOM model.
//
// Partial pooling across visa categories on trend-innovation precision;
// time-varying classification probabilities via a GP prior with a 16-
// quarter length-scale (so π is smooth over a year or two). Vintage-
// aware NOM observation block: preliminary releases get a wider error
// variance than revised/final ones.
//
// This file is a working scaffold for Phase 3. The model compiles and
// implements the structural specification but has not been calibrated
// against the v1 panel — see reports/methodology.qmd Section 6 for the
// validation plan before relying on it in production.

functions {
  vector gp_exp_quad_cov_vec(int N, real alpha, real rho) {
    vector[N] out;
    for (i in 1:N)
      out[i] = alpha^2 * exp(-0.5 * square(i - 1) / square(rho));
    return out;
  }
}

data {
  int<lower=1> N_cat;
  int<lower=1> N_t;
  int<lower=1> N_obs;
  array[N_obs] int<lower=1, upper=N_cat> cat_idx;
  array[N_obs] int<lower=1, upper=N_t>   t_idx;

  vector[N_obs] y_log_arrivals;
  vector[N_obs] y_visa_grants;
  vector[N_obs] y_nom_obs;

  // Vintage age (quarters since reference period at observation time)
  array[N_obs] int<lower=0> vintage_age;
}

parameters {
  // Latent log-arrivals state by (category, quarter)
  matrix[N_cat, N_t] mu;        // level
  matrix[N_cat, N_t] delta;     // slope

  // Partial-pooling hyperparameters
  real<lower=0> sigma_level;
  real<lower=0> sigma_slope;
  vector<lower=0>[N_cat] sigma_level_c;
  vector<lower=0>[N_cat] sigma_slope_c;

  // Visa-grants observation block
  vector<lower=0>[N_cat] kappa_v;
  vector<lower=0>[N_cat] sigma_v;

  // Classification probabilities pi[c, t] via GP prior on the logit
  matrix[N_cat, N_t] logit_pi;
  real<lower=0> pi_alpha;
  real<lower=0> pi_rho;

  // NOM observation block — vintage-age scaling on error variance
  real<lower=0> sigma_nom_floor;
  real<lower=0> sigma_nom_decay;
}

transformed parameters {
  matrix[N_cat, N_t] pi_t = inv_logit(logit_pi);
}

model {
  // Priors
  sigma_level ~ normal(0, 1);
  sigma_slope ~ normal(0, 0.5);
  sigma_level_c ~ normal(sigma_level, 0.5);
  sigma_slope_c ~ normal(sigma_slope, 0.25);
  kappa_v ~ normal(0.5, 0.5);
  sigma_v ~ normal(0, 1);
  pi_alpha ~ normal(0, 1);
  pi_rho   ~ normal(16, 4);
  sigma_nom_floor ~ normal(0, 5000);
  sigma_nom_decay ~ normal(0, 5);

  // Latent trend dynamics (local linear)
  for (c in 1:N_cat) {
    delta[c, 1] ~ normal(0, sigma_slope_c[c]);
    mu[c, 1]    ~ normal(0, 5);
    for (t in 2:N_t) {
      delta[c, t] ~ normal(delta[c, t - 1], sigma_slope_c[c]);
      mu[c, t]    ~ normal(mu[c, t - 1] + delta[c, t - 1], sigma_level_c[c]);
    }
  }

  // GP-smoothed logit-pi
  // (kernel implemented as random walk for tractability in this scaffold)
  for (c in 1:N_cat) {
    logit_pi[c, 1] ~ normal(0, pi_alpha);
    for (t in 2:N_t)
      logit_pi[c, t] ~ normal(logit_pi[c, t - 1], pi_alpha / pi_rho);
  }

  // Observation likelihoods
  for (i in 1:N_obs) {
    int c = cat_idx[i];
    int t = t_idx[i];
    real arr_hat = exp(mu[c, t]);

    if (y_log_arrivals[i] != -1)
      y_log_arrivals[i] ~ normal(mu[c, t], 0.1);

    if (y_visa_grants[i] != -1)
      y_visa_grants[i] ~ normal(kappa_v[c] * arr_hat, sigma_v[c]);

    if (y_nom_obs[i] != -1) {
      real sd_nom = sigma_nom_floor *
                    (1 + sigma_nom_decay / (1.0 + vintage_age[i]));
      y_nom_obs[i] ~ normal(pi_t[c, t] * arr_hat, sd_nom);
    }
  }
}

generated quantities {
  matrix[N_cat, N_t] nom_pred;
  for (c in 1:N_cat)
    for (t in 1:N_t)
      nom_pred[c, t] = pi_t[c, t] * exp(mu[c, t]);
}
