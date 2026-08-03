data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  vector[2] mu;
}
model {
  mu ~ normal(0, 1);
  y ~ normal(mu[1] + mu[2], 1);
}
generated quantities {
  real mu_sum = mu[1] + mu[2];
}
