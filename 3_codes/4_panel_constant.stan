
// -----------------------------------------------------------------------------
// About this code
// Estimate a logit model with hierarchy
// coefficients are still fixed (not time-varying)
// -----------------------------------------------------------------------------

// input data
data {
  int  I;                          // sample size
  int  C;                          // number of country (18 countries)
  real Cre_d[I];                   // domestic credit
  real Cre_g[I];                   // global credit
  real Slo_d[I];                   // domestic slope
  real Slo_g[I];                   // global slope
  int<lower=0, upper=1> Crisis[I]; // crisis dummy
  int<lower=1, upper=C> CID[I];    // country index of each sample
}

// parameters accepted by the model
parameters {
  real b_i; // intercept
  real b_cd[C]; // credit domestic
  real b_cg[C]; // credit global
  real b_sd[C]; // slope domestic
  real b_sg[C]; // slope global
  real mu_cd; // mu of b_cd
  real mu_cg; // mu of b_cg
  real mu_sd; // mu of b_sd
  real mu_sg; // mu of b_sg
  real<lower=0> s_cd; // sd of the b_cd
  real<lower=0> s_cg; // sd of the b_cg
  real<lower=0> s_sd; // sd of the b_sd
  real<lower=0> s_sg; // sd of the b_sg
}

// transformed parameters
transformed  parameters {
  real q[I];
  for (i in 1:I)
    q[i]  = inv_logit(b_i + b_cd[CID[i]] * Cre_d[i] + b_cg[CID[i]] * Cre_g[i] + b_sd[CID[i]] * Slo_d[i] + b_sg[CID[i]] * Slo_g[i]);
}

// model to be estimated
model {
  for (c in 1:C) {
    b_cd[c] ~ normal(mu_cd, s_cd);
    b_cg[c] ~ normal(mu_cg, s_cg);
    b_sd[c] ~ normal(mu_sd, s_sd);
    b_sg[c] ~ normal(mu_sg, s_sg);
  }
  for (i in 1:I)
    Crisis[i] ~ bernoulli(q[i]);
}

