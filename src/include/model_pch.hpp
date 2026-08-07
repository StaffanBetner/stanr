#ifndef STANR_MODEL_PCH_HPP
#define STANR_MODEL_PCH_HPP
// Order mirrors the assembled model TU: stanc code includes model_header
// first, then the wrapper includes Rcpp and stanr headers.
#include <stan/model/model_header.hpp>
#include <Rcpp.h>
#include <stanr/r_data_context.hpp>
#include <stanr/model_methods.hpp>
#endif
