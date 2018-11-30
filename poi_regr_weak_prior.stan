data {
  int<lower=0> N;                 // num observations
  int<lower=0> y[N];              // count outcomes
  int<lower=1> K;                 // num covariates
  matrix[N, K] x;                 // design matrix
}
parameters {
  real beta0;            // intercept
  vector[K] betas;       // covariates
}
model {
  y ~ poisson_log(beta0 + x * betas);  

  beta0 ~ student_t(1,0,2.5); // student_t(nu,0,s) where 3<nu<7 equiv to Cauchy prior,
  betas ~ student_t(1,0,2.5); // took it from Prior Choice Recommendations in Stan-Wiki
}
generated quantities {
  vector[N] eta = beta0 + x * betas; // co-variates
  vector[N] mu = exp(eta);
}
