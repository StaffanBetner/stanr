// Unbounded tuple/complex parameters so unconstrain_variables() is the
// identity transform -- used to test init/unconstrain round trips for
// tuple and complex parameters (test-tuple-data.R).
parameters {
  tuple(real, vector[2]) t;
  complex z;
}
model {
  t.1 ~ std_normal();
  t.2 ~ std_normal();
  get_real(z) ~ std_normal();
  get_imag(z) ~ std_normal();
}
