// Phase 3 v0.1 — Bayesian state-space on headline (total) NOM.
//
// Parameterisation note: NCP on state innovations. The joint
// posterior has a sigma_oad ↔ sigma_mu identifiability funnel
// (corr ~ -0.7) that saturates NUTS' max_treedepth. Centred
// parameterisation breaks convergence on sigma_beta; dense
// mass matrix gets stuck across modes. NCP + adapt_delta 0.95
// + iter_warmup 500 + iter_sampling 500 gets divergences down
// to ~1% with ESS ~100 on the slow parameters — usable, with
// the treedepth warning being intrinsic cost-of-funnel.
// Proper fix (a Phase 3 v0.6 follow-up) is to marginalise the
// latent states by writing the Kalman filter directly in Stan.
//
// Same observation structure as fit_kalman_multi_one but with proper
// posterior sampling instead of an MLE fit. One latent log-arrivals
// state μ_t with a damped local-linear trend; three observation
// equations on log scale:
//
//   y_oad[t]  = μ_t              + ε_o
//   y_visa[t] = μ_{t - ℓ} + α_v  + ε_v   (ℓ = lead quarters)
//   y_nom[t]  = μ_t       + α_n  + ε_n
//
// Transition (non-centered to keep the funnel between σ_state and
// the state innovations from breaking HMC's adaptation):
//
//   mu_raw[t]    ~ N(0, 1)
//   beta_raw[t]  ~ N(0, 1)
//   μ_t          = μ_{t-1} + β_{t-1} + σ_μ * mu_raw[t]
//   β_t          = φ * β_{t-1}      + σ_β * beta_raw[t]
//
// All observations are optional per period (use the has_* mask).
// Sample-from-prior diagnostics: set `prior_only = 1` in data.

data {
  int<lower=1> T;                          // total panel length (incl forecast)
  int<lower=1> T_obs;                      // last observed period
  int<lower=1> lead_q;                     // visa-grants lead
  array[T_obs] real y_oad;                 // log OAD long-term arrivals
  array[T_obs] int<lower=0, upper=1> has_oad;
  array[T_obs] real y_visa;                // log lagged visa-grants
  array[T_obs] int<lower=0, upper=1> has_visa;
  array[T_obs] real y_nom;                 // log NOM
  array[T_obs] int<lower=0, upper=1> has_nom;
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

parameters {
  real mu_init;
  real beta_init;
  vector[T - 1] mu_raw;                    // standardised level innovations
  vector[T - 1] beta_raw;                  // standardised slope innovations
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
  // ---------- priors
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

  // ---------- observations
  if (!prior_only) {
    for (t in 1:T_obs) {
      if (has_oad[t])  y_oad[t]  ~ normal(mu[t],            sigma_oad);
      if (has_nom[t])  y_nom[t]  ~ normal(mu[t] + alpha_n,  sigma_nom);
      if (has_visa[t] && t > lead_q) {
        y_visa[t] ~ normal(mu[t - lead_q] + alpha_v, sigma_visa);
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
