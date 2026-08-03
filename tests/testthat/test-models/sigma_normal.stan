data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
}
model {
  sigma ~ normal(0, 1);
  y ~ normal(0, sigma);
}
