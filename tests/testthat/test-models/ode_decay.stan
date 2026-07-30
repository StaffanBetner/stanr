functions {
  vector decay_rhs(real t, vector y, array[] real theta,
                   array[] real x_r, array[] int x_i) {
    vector[1] dydt;
    dydt[1] = -theta[1] * y[1];
    return dydt;
  }
}
data {
  int<lower=1> T;
  array[T] real<lower=0> ts;
  real<lower=0> y0;
  real<lower=0> rate;
}
transformed data {
  array[1] real theta = {rate};
  array[0] real x_r;
  array[0] int x_i;
  array[T] vector[1] solution = ode_rk45(
    decay_rhs, [y0]', 0, ts, theta, x_r, x_i
  );
}
model {}
generated quantities {
  vector[T] concentration;
  for (t in 1:T) {
    concentration[t] = solution[t][1];
  }
}
