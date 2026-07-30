functions {
  real partial_normal_lpdf(array[] real y_slice, int start, int end,
                          real mu, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }
}
data {
  int<lower=1> N;
  array[N] real y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 5);
  sigma ~ exponential(1);
  target += reduce_sum(partial_normal_lpdf, y, 1, mu, sigma);
}
generated quantities {
  real log_lik = normal_lpdf(y | mu, sigma);
}
