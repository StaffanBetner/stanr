real shifted_normal_lpdf(real y, real mu, real sigma) {
  return normal_lpdf(y | mu, sigma);
}
