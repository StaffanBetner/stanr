data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] X;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real alpha;
  vector[K] beta;
}
model {
  alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  y ~ bernoulli_logit_glm(X, alpha, beta);
}
