// Phase 3 v0.6 — Bayesian headline SSM with Kalman-filter likelihood.
// Phase 3 v1.0 — optional quarterly seasonal states (use_seasonal = 1).
//
// Same observation streams as bayes_gamma (OAD, visa-grants, NOM)
// but the latent state is integrated out analytically via a forward
// Kalman filter, leaving only ~10-12 static parameters to sample.
// Eliminates the sigma_oad ↔ sigma_mu funnel that saturates HMC tree
// depth in the latent-state versions.
//
// Two material differences from bayes_gamma:
//   1. Visa observation is linearised from log_sum_exp to
//      sum_k w_k * mu[lag_k]. (Exact when arrivals are flat
//      across the lag window.)
//   2. Visa lag is BACKWARD in calendar time — visa observed at
//      quarter t informs arrivals at quarters t, t-1, ..., t-K_max.
//
// Seasonality (v1.0). Without it the model has no way to represent
// the large within-year swings in ABS NOM (post-COVID the April-June
// and October-December quarters run at roughly half the level of
// January-March and July-September), so every nowcast collapses to a
// smoothed annual level and overshoots the low quarters by 30-50k.
// With use_seasonal = 1 the state carries two slowly evolving
// quarterly dummy seasonals on the log scale — one for OAD long-term
// arrivals, one for NOM — because the two have different within-year
// shapes (NOM nets out departures and the 12/16-month classification).
// Visa grants are annual FY totals spread over quarters in proportion
// to OAD arrivals, so (visa_seasonal = 1) they load on the OAD
// seasonal too. mu_t is then a deseasonalised level.
//
// State at filter step t (K = K_max, s = 3 + K):
//   x_t = [ mu_t, beta_t, mu_{t-1}, ..., mu_{t-K},
//           g_oad_t, g_oad_{t-1}, g_oad_{t-2},      (if use_seasonal)
//           g_nom_t, g_nom_{t-1}, g_nom_{t-2} ]     (if use_seasonal)
// Transition:
//   mu_t    = mu_{t-1} + beta_{t-1} + N(0, sigma_mu^2)
//   beta_t  = phi * beta_{t-1}      + N(0, sigma_beta^2)
//   lag_k_t = lag_{k-1, t-1}
//   g_t     = -(g_{t-1} + g_{t-2} + g_{t-3}) + N(0, sigma_seas^2)
// Observation at step t:
//   OAD  : y          = mu_t + g_oad_t                          + eps
//   NOM  : y - alpha_n = mu_t + g_nom_t                         + eps
//   Visa : y - alpha_v = sum_k w_k mu_{t-K+k} (+ g_oad_t)        + eps
//                                         (when has_visa and t > K_max)

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
  int<lower=0, upper=1> use_seasonal;
  int<lower=0, upper=1> visa_seasonal;
  real<lower=0> seas0_scale;           // prior SD of initial seasonal states
  real<lower=0> sigma_seas_scale;      // half-normal scale, seasonal innovations
  int<lower=0, upper=1> prior_only;
}

transformed data {
  int S = 6 * use_seasonal;
  int<lower=2> M = 2 + K_max + S;
  int s_oad = 3 + K_max;               // first OAD seasonal state
  int s_nom = 6 + K_max;               // first NOM seasonal state
  vector[M] z_oad = rep_vector(0, M);
  vector[M] z_nom = rep_vector(0, M);
  vector[M] z_visa = rep_vector(0, M);
  z_oad[1] = 1;
  z_nom[1] = 1;
  z_visa[1] = gamma_weights[n_w];
  for (k in 1:K_max) z_visa[2 + k] = gamma_weights[n_w - k];
  if (use_seasonal) {
    z_oad[s_oad] = 1;
    z_nom[s_nom] = 1;
    if (visa_seasonal) z_visa[s_oad] = 1;
  }
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
  vector<lower=0>[2 * use_seasonal] sigma_seas;   // [1] OAD, [2] NOM
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
  if (use_seasonal) {
    for (b in 1:2) {
      int s = s_oad + 3 * (b - 1);
      F[s, s] = -1;  F[s, s + 1] = -1;  F[s, s + 2] = -1;
      F[s + 1, s] = 1;
      F[s + 2, s + 1] = 1;
      Q[s, s] = square(sigma_seas[b]);
    }
  }
}

model {
  alpha_v    ~ normal(0, alpha_scale);
  alpha_n    ~ normal(0, alpha_scale);
  sigma_oad  ~ normal(0, sigma_obs_scale);
  sigma_visa ~ normal(0, sigma_obs_scale);
  sigma_nom  ~ normal(0, sigma_obs_scale);
  sigma_mu   ~ normal(0, sigma_state_scale);
  sigma_beta ~ normal(0, sigma_state_scale);
  sigma_seas ~ normal(0, sigma_seas_scale);
  phi ~ beta(phi_loc * phi_conc, (1 - phi_loc) * phi_conc);
  mu_init   ~ normal(mu0_loc, mu0_scale);
  beta_init ~ normal(0, slope0_scale);

  if (!prior_only) {
    vector[M] x = rep_vector(mu_init, M);
    x[2] = beta_init;
    matrix[M, M] P = diag_matrix(rep_vector(square(mu0_scale), M));
    P[2, 2] = square(slope0_scale);
    if (use_seasonal) {
      for (j in s_oad:M) { x[j] = 0; P[j, j] = square(seas0_scale); }
    }

    real log_lik = 0;
    for (t in 1:T_obs) {
      x = F * x;
      P = F * P * F' + Q;
      P = 0.5 * (P + P');

      if (has_oad[t]) {
        real S_t = quad_form_sym(P, z_oad) + square(sigma_oad);
        real innov = y_oad[t] - dot_product(z_oad, x);
        vector[M] K_gain = (P * z_oad) / S_t;
        x = x + K_gain * innov;
        P = P - K_gain * (z_oad' * P);
        P = 0.5 * (P + P');
        log_lik += -0.5 * (log(2 * pi() * S_t) + square(innov) / S_t);
      }
      if (has_nom[t]) {
        real S_t = quad_form_sym(P, z_nom) + square(sigma_nom);
        real innov = y_nom[t] - alpha_n - dot_product(z_nom, x);
        vector[M] K_gain = (P * z_nom) / S_t;
        x = x + K_gain * innov;
        P = P - K_gain * (z_nom' * P);
        P = 0.5 * (P + P');
        log_lik += -0.5 * (log(2 * pi() * S_t) + square(innov) / S_t);
      }
      if (has_visa[t] && t > K_max) {
        real S_t = quad_form_sym(P, z_visa) + square(sigma_visa);
        real innov = y_visa[t] - alpha_v - dot_product(z_visa, x);
        vector[M] K_gain = (P * z_visa) / S_t;
        x = x + K_gain * innov;
        P = P - K_gain * (z_visa' * P);
        P = 0.5 * (P + P');
        log_lik += -0.5 * (log(2 * pi() * S_t) + square(innov) / S_t);
      }
    }
    target += log_lik;
  }
}

generated quantities {
  // Filtered posterior summary at every in-sample step (one-sided,
  // observations up to t) and predicted values for t > T_obs. The NOM
  // estimate is the NOM observation's signal z_nom' x, i.e. level plus
  // the NOM seasonal when use_seasonal = 1.
  //
  // v0.7 explored an RTS smoother here; empirically worse on this
  // panel. Kept as a logged negative result; see Phase 3 history.
  vector[T_obs + H] nom_hat_log_mean;
  vector[T_obs + H] nom_hat_log_sd;
  vector[T_obs + H] nom_hat;
  {
    vector[M] x = rep_vector(mu_init, M);
    x[2] = beta_init;
    matrix[M, M] P = diag_matrix(rep_vector(square(mu0_scale), M));
    P[2, 2] = square(slope0_scale);
    if (use_seasonal) {
      for (j in s_oad:M) { x[j] = 0; P[j, j] = square(seas0_scale); }
    }
    for (t in 1:T_obs) {
      x = F * x;
      P = F * P * F' + Q;
      P = 0.5 * (P + P');
      if (has_oad[t]) {
        real S_t = quad_form_sym(P, z_oad) + square(sigma_oad);
        vector[M] K_gain = (P * z_oad) / S_t;
        x = x + K_gain * (y_oad[t] - dot_product(z_oad, x));
        P = P - K_gain * (z_oad' * P);
        P = 0.5 * (P + P');
      }
      if (has_nom[t]) {
        real S_t = quad_form_sym(P, z_nom) + square(sigma_nom);
        vector[M] K_gain = (P * z_nom) / S_t;
        x = x + K_gain * (y_nom[t] - alpha_n - dot_product(z_nom, x));
        P = P - K_gain * (z_nom' * P);
        P = 0.5 * (P + P');
      }
      if (has_visa[t] && t > K_max) {
        real S_t = quad_form_sym(P, z_visa) + square(sigma_visa);
        vector[M] K_gain = (P * z_visa) / S_t;
        x = x + K_gain * (y_visa[t] - alpha_v - dot_product(z_visa, x));
        P = P - K_gain * (z_visa' * P);
        P = 0.5 * (P + P');
      }
      {
        real m_t = dot_product(z_nom, x);
        real v_t = fmax(quad_form_sym(P, z_nom), 0);
        nom_hat_log_mean[t] = m_t + alpha_n;
        nom_hat_log_sd[t]   = sqrt(v_t + square(sigma_nom));
        nom_hat[t] = exp(nom_hat_log_mean[t] + 0.5 * v_t);
      }
    }
    for (h in 1:H) {
      x = F * x;
      P = F * P * F' + Q;
      P = 0.5 * (P + P');
      real m_h = dot_product(z_nom, x);
      real v_h = fmax(quad_form_sym(P, z_nom), 0);
      nom_hat_log_mean[T_obs + h] = m_h + alpha_n;
      nom_hat_log_sd[T_obs + h]   = sqrt(v_h + square(sigma_nom));
      nom_hat[T_obs + h] = exp(nom_hat_log_mean[T_obs + h] + 0.5 * v_h);
    }
  }
}
