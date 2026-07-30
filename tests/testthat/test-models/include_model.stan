functions {
  #include "nested_helpers.stan"
}
data {
  real y;
}
parameters {
  real mu;
}
model {
  target += shifted_normal_lpdf(centered(y) | mu, 1);
  mu ~ normal(0, 1);
}
