data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
  real mu;
}
parameters {
  real<lower=0, upper=1> theta;
  vector[2] beta;
}
transformed parameters {
  real beta_sum = sum(beta);
}
model {
  theta ~ beta(1, 1);
  y ~ bernoulli(theta);
  beta ~ normal(mu, 1);
}
generated quantities {
  real deterministic_gq = theta + beta_sum;
  real stochastic_gq = normal_rng(beta_sum, 1);
}
