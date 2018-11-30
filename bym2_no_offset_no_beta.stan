data {
  int<lower=0> N;
  int<lower=0> N_edges;
  int<lower=1, upper=N> node1[N_edges];  // node1[i] adjacent to node2[i]
  int<lower=1, upper=N> node2[N_edges];  // and node1[i] < node2[i]

  int<lower=0> y[N];              // count outcomes

  real<lower=0> scaling_factor; // scales the variance of the spatial effects
}
parameters {
  real beta0;            // intercept

  real<lower=0> sigma;        // overall standard deviation
  real<lower=0, upper=1> rho; // proportion unstructured vs. spatially structured variance

  vector[N] theta;       // heterogeneous effects
  vector[N] phi;         // spatial effects
}
transformed parameters {
  vector[N] convolved_re;
  // variance of each component should be approximately equal to 1
  convolved_re =  sqrt(1 - rho) * theta + sqrt(rho / scaling_factor) * phi;
}
model {
  y ~ poisson_log(beta0 + convolved_re * sigma);  // co-variates

  // This is the prior for phi! (up to proportionality)
  target += -0.5 * dot_self(phi[node1] - phi[node2]);

  beta0 ~ normal(0.0, 2.5); // comment to get non-info priors
  theta ~ normal(0.0, 1.0);
  sigma ~ normal(0, 1.0);
  rho ~ beta(0.5, 0.5);
  // soft sum-to-zero constraint on phi)
  sum(phi) ~ normal(0, 0.001 * N);  // equivalent to mean(phi) ~ normal(0,0.001)
}
generated quantities {
  real logit_rho = log(rho / (1.0 - rho));
  vector[N] eta = beta0 + convolved_re * sigma; // co-variates
  vector[N] mu = exp(eta);
}
