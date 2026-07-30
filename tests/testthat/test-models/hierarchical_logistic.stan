data {
  int<lower=1> N;
  int<lower=1> K;
  int<lower=1> G;
  matrix[N, K] X;
  array[N] int<lower=1, upper=G> group;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  vector[K] beta;
  vector<lower=0>[K] tau;
  cholesky_factor_corr[K] L_Omega;
  matrix[K, G] z_group;
}
transformed parameters {
  matrix[K, G] beta_group = diag_pre_multiply(tau, L_Omega) * z_group;
}
model {
  to_vector(z_group) ~ std_normal();
  beta ~ normal(0, 2);
  tau ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(X[n] * (beta + beta_group[, group[n]]));
  }
}
generated quantities {
  corr_matrix[K] Omega = multiply_lower_tri_self_transpose(L_Omega);
  array[N] int y_rep;
  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(X[n] * (beta + beta_group[, group[n]]));
  }
}
