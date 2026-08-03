// Codegen-contract battery: pins the A2-A4 stanc-2.39 var_context reading
// contracts (blocked-AoS tuple-array flattening, windowed vals_c for
// tuple-slot complex leaves) against upgrades. Every input is echoed
// unchanged via generated quantities; test-tuple-data.R asserts exact
// equality between the supplied data and every echoed draw column.
data {
  complex zd;
  complex_vector[2] zv;
  complex_matrix[2, 2] zm;
  array[2] complex za;
  tuple(real, vector[2]) td;
  array[2] tuple(int, complex) tad;
  array[2] tuple(complex_vector[3], real) acv;
  array[2, 2] tuple(int, real) t2d;
  tuple(real, array[2] tuple(real, complex)) nt;
}
parameters {
  real x;
}
model {
  x ~ std_normal();
}
generated quantities {
  complex zd_out = zd;
  complex_vector[2] zv_out = zv;
  complex_matrix[2, 2] zm_out = zm;
  array[2] complex za_out = za;
  tuple(real, vector[2]) td_out = td;
  array[2] tuple(int, complex) tad_out = tad;
  array[2] tuple(complex_vector[3], real) acv_out = acv;
  array[2, 2] tuple(int, real) t2d_out = t2d;
  tuple(real, array[2] tuple(real, complex)) nt_out = nt;
}
