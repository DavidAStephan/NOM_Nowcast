// Phase 3 v0.6 — Bayesian headline SSM with Kalman-filter likelihood.
//
// Same observation streams as bayes_gamma (OAD, visa-grants, NOM)
// but the latent state {mu_t, beta_t, mu_{t-1}, ..., mu_{t-K_max}}
// is integrated out analytically via a forward Kalman filter,
// leaving only ~10 static parameters to sample. Eliminates the
// sigma_oad ↔ sigma_mu funnel that saturates HMC tree depth in
// the latent-state versions.
//
// Two material differences from bayes_gamma:
//   1. Visa observation is linearised from log_sum_exp to
//      sum_k w_k * mu[lag_k]. (Exact when arrivals are flat
//      across the lag window.)
//   2. Visa lag is BACKWARD in calendar time — visa observed at
//      quarter t informs arrivals at quarters t, t-1, ..., t-K_max.
//      bayes_gamma uses the forward convention (visa at t leads
//      arrivals at t..t+K_max). The forward convention is not
//      cleanly expressible in a causal forward Kalman without
//      smoothing; we treat the two as competing model variants
//      and let the backtest pick the winner.
//
// State at filter step t:
//   x_t = [ mu_t, beta_t, mu_{t-1}, mu_{t-2}, ..., mu_{t-K_max} ]
// Transition:
//   mu_t      = mu_{t-1} + beta_{t-1} + N(0, sigma_mu^2)
//   beta_t    = phi * beta_{t-1}      + N(0, sigma_beta^2)
//   lag_k_t   = lag_{k-1, t-1}                              (k >= 1)
//   lag_1     ≡ mu_{t-1}
// Observation at filter step t (= calendar t):
//   OAD  : y = mu_t + N(0, sigma_oad^2)              when has_oad
//   NOM  : y - alpha_n = mu_t + N(0, sigma_nom^2)    when has_nom
//   Visa : y - alpha_v
//        = w[K]*mu_t + w[K-1]*lag_1 + ... + w[0]*lag_K + N(0, sigma_visa^2)
//                                                    when has_visa and t > K_max

data {
  int<lower=1> T_obs;
  int<lower=1> H;
  int<lower=1> K_max;
  int<lower=0> n_w;                    // n_w = K_max + 1
  array[T_obs] real y_oad;
  array[T_obs] int<lower=0, upper=1> has_oad;
  array[T_obs] real y_nom;
  array[T_obs] int<lower=0, upper=1> has_nom;
  array[T_obs] real y_visa;
  array[T_obs] int<lower=0, upper=1> has_visa;
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
  int<lower=2> M = 2 + K_max;
  vector[M] z_oad = rep_vector(0, M);
  z_oad[1] = 1;
  vector[M] z_nom_lin = z_oad;
  vector[M] z_visa = rep_vector(0, M);
  z_visa[1] = gamma_weights[n_w];
  for (k in 1:K_max) z_visa[2 + k] = gamma_weights[n_w - k];
}

parameters {
  real alpha_v;
  real alpha_n;
  real<lower=0> sigma_oad;
  real<lower=0> sigma_visa;
  real<lower=0> sigma_nom;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_beta;
  real<lower=0, upper=1> phi;
  real mu_init;
  real beta_init;
}

transformed parameters {
  matrix[M, M] F = rep_matrix(0, M, M);
  matrix[M, M] Q = rep_matrix(0, M, M);
  F[1, 1] = 1;  F[1, 2] = 1;
  F[2, 2] = phi;
  F[3, 1] = 1;
  for (k in 2:K_max) F[2 + k, 2 + k - 1] = 1;
  Q[1, 1] = square(sigma_mu);
  Q[2, 2] = square(sigma_beta);
}

model {
  alpha_v    ~ normal(0, alpha_scale);
  alpha_n    ~ normal(0, alpha_scale);
  sigma_oad  ~ normal(0, sigma_obs_scale);
  sigma_visa ~ normal(0, sigma_obs_scale);
  sigma_nom  ~ normal(0, sigma_obs_scale);
  sigma_mu   ~ normal(0, sigma_state_scale);
  sigma_beta ~ normal(0, sigma_state_scale);
  phi ~ beta(phi_loc * phi_conc, (1 - phi_loc) * phi_conc);
  mu_init   ~ normal(mu0_loc, mu0_scale);
  beta_init ~ normal(0, slope0_scale);

  if (!prior_only) {
    vector[M] x = rep_vector(mu_init, M);
    x[2] = beta_init;
    matrix[M, M] P = diag_matrix(rep_vector(square(mu0_scale), M));
    P[2, 2] = square(slope0_scale);

    real log_lik = 0;
    for (t in 1:T_obs) {
      x = F * x;
      P = F * P * F' + Q;
      P = 0.5 * (P + P');

      if (has_oad[t]) {
        real S = quad_form_sym(P, z_oad) + square(sigma_oad);
        real innov = y_oad[t] - dot_product(z_oad, x);
        vector[M] K_gain = (P * z_oad) / S;
        x = x + K_gain * innov;
        P = P - K_gain * (z_oad' * P);
        P = 0.5 * (P + P');
        log_lik += -0.5 * (log(2 * pi() * S) + square(innov) / S);
      }
      if (has_nom[t]) {
        real S = quad_form_sym(P, z_nom_lin) + square(sigma_nom);
        real innov = y_nom[t] - alpha_n - dot_product(z_nom_lin, x);
        vector[M] K_gain = (P * z_nom_lin) / S;
        x = x + K_gain * innov;
        P = P - K_gain * (z_nom_lin' * P);
        P = 0.5 * (P + P');
        log_lik += -0.5 * (log(2 * pi() * S) + square(innov) / S);
      }
      if (has_visa[t] && t > K_max) {
        real S = quad_form_sym(P, z_visa) + square(sigma_visa);
        real innov = y_visa[t] - alpha_v - dot_product(z_visa, x);
        vector[M] K_gain = (P * z_visa) / S;
        x = x + K_gain * innov;
        P = P - K_gain * (z_visa' * P);
        P = 0.5 * (P + P');
        log_lik += -0.5 * (log(2 * pi() * S) + square(innov) / S);
      }
    }
    target += log_lik;
  }
}

generated quantities {
  // Emit a posterior summary at every timestep — filtered for
  // t <= T_obs (one-sided, using observations up to t), predicted
  // for t > T_obs. This lets the backtest score historical quarters
  // as well as future forecast horizons.
  vector[T_obs + H] nom_hat_log_mean;
  vector[T_obs + H] nom_hat_log_sd;
  vector[T_obs + H] nom_hat;
  {
    vector[M] x = rep_vector(mu_init, M);
    x[2] = beta_init;
    matrix[M, M] P = diag_matrix(rep_vector(square(mu0_scale), M));
    P[2, 2] = square(slope0_scale);
    for (t in 1:T_obs) {
      x = F * x;
      P = F * P * F' + Q;
      P = 0.5 * (P + P');
      if (has_oad[t]) {
        real S = quad_form_sym(P, z_oad) + square(sigma_oad);
        vector[M] K_gain = (P * z_oad) / S;
        x = x + K_gain * (y_oad[t] - dot_product(z_oad, x));
        P = P - K_gain * (z_oad' * P);
        P = 0.5 * (P + P');
      }
      if (has_nom[t]) {
        real S = quad_form_sym(P, z_nom_lin) + square(sigma_nom);
        vector[M] K_gain = (P * z_nom_lin) / S;
        x = x + K_gain * (y_nom[t] - alpha_n - dot_product(z_nom_lin, x));
        P = P - K_gain * (z_nom_lin' * P);
        P = 0.5 * (P + P');
      }
      if (has_visa[t] && t > K_max) {
        real S = quad_form_sym(P, z_visa) + square(sigma_visa);
        vector[M] K_gain = (P * z_visa) / S;
        x = x + K_gain * (y_visa[t] - alpha_v - dot_product(z_visa, x));
        P = P - K_gain * (z_visa' * P);
        P = 0.5 * (P + P');
      }
      // Record filtered mu_t (one-sided posterior using obs up to t)
      real mu_filt = x[1];
      real var_filt = P[1, 1];
      nom_hat_log_mean[t] = mu_filt + alpha_n;
      nom_hat_log_sd[t]   = sqrt(var_filt + square(sigma_nom));
      nom_hat[t] = exp(nom_hat_log_mean[t] + 0.5 * var_filt);
    }
    // Forecast forward
    for (h in 1:H) {
      x = F * x;
      P = F * P * F' + Q;
      P = 0.5 * (P + P');
      real mu_h = x[1];
      real var_state = P[1, 1];
      nom_hat_log_mean[T_obs + h] = mu_h + alpha_n;
      nom_hat_log_sd[T_obs + h]   = sqrt(var_state + square(sigma_nom));
      nom_hat[T_obs + h] = exp(nom_hat_log_mean[T_obs + h] + 0.5 * var_state);
    }
  }
}
