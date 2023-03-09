
// -----------------------------------------------------------------------------
// About this code
// Estimate a simple logit model without hierarchy
// -----------------------------------------------------------------------------

// input data
data {
  int  I;                          // sample size
  int  Time;                       // number of time variables
  real Cre_d[I];                   // domestic credit
  real Cre_g[I];                   // global credit
  real Slo_d[I];                   // domestic slope
  real Slo_g[I];                   // global slope
  int<lower=0, upper=1> Crisis[I]; // crisis dummy
  int<lower=1, upper=Time> TID[I]; // time index of each sample
  real b_i_pri;                    // prior mean of the intercept
  real b_cd_pri;                   // prior mean of the b_cd
  real b_cg_pri;                   // prior mean of the b_cg
  real b_sd_pri;                   // prior mean of the b_sd
  real b_sg_pri;                   // prior mean of the b_sg
}

// parameters accepted by the model
parameters {
  real b_i[Time];  // intercept
  real b_cd[Time]; // credit domestic
  real b_cg[Time]; // credit global
  real b_sd[Time]; // slope domestic
  real b_sg[Time]; // slope global
  real<lower=0> s_i;  // sd of b_i
  real<lower=0> s_cd; // sd of b_cd
  real<lower=0> s_cg; // sd of b_cg
  real<lower=0> s_sd; // sd of b_sd
  real<lower=0> s_sg; // sd of b_sg
  }

// transformed parameters
transformed  parameters {
  real q[I];
  for (i in 1:I)
    q[i]  = inv_logit(b_i[TID[i]] + b_cd[TID[i]] * Cre_d[i] + b_cg[TID[i]] * Cre_g[i] + b_sd[TID[i]] * Slo_d[i] + b_sg[TID[i]] * Slo_g[i]);
}

// model to be estimated
model {
  // likelihood block: now let coefficients evolve over time
  for (t in 2:Time) {
    b_i[t]  ~ normal(b_i[t-1],  s_i);  // intercept
    b_cd[t] ~ normal(b_cd[t-1], s_cd); // credit domestic
    b_cg[t] ~ normal(b_cg[t-1], s_cg); // credit global
    b_sd[t] ~ normal(b_sd[t-1], s_sd); // slope domestic
    b_sg[t] ~ normal(b_sg[t-1], s_sg); // slope global
  }
  for (i in 1:I) {
    Crisis[i] ~ bernoulli(q[i]); // logistic regression
  }
  // priors block: assume that the coefs of the first period is distributed around the full-sample coefs
  b_i[1]  ~ normal(b_i_pri,  0.5);
  b_cd[1] ~ normal(b_cd_pri, 0.5);
  b_cg[1] ~ normal(b_cg_pri, 0.5);
  b_sd[1] ~ normal(b_sd_pri, 0.5);
  b_sg[1] ~ normal(b_sg_pri, 0.5);
  }

