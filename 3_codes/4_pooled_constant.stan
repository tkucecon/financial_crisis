
// -----------------------------------------------------------------------------
// About this code
// Estimate a simple logit model without hierarchy
// -----------------------------------------------------------------------------

// input data
data {
  int  I;                          // sample size
  real Cre_d[I];                   // domestic credit
  real Cre_g[I];                   // global credit
  real Slo_d[I];                   // domestic slope
  real Slo_g[I];                   // global slope
  int<lower=0, upper=1> Crisis[I]; // crisis dummy
}

// parameters accepted by the model
parameters {
  real b[5]; // parameters as a vector
}

// transformed parameters
transformed  parameters {
  real q[I];
  for (i in 1:I)
    q[i]  = inv_logit(b[1] + b[2] * Cre_d[i] + b[3] * Cre_g[i] + b[4] * Slo_d[i] + b[5] * Slo_g[i]);
}

// model to be estimated
model {
  for (i in 1:I)
    Crisis[i] ~ bernoulli(q[i]);
}

