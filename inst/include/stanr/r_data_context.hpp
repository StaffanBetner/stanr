#ifndef STANR_R_DATA_CONTEXT_HPP
#define STANR_R_DATA_CONTEXT_HPP

#include <Rcpp.h>
#include <stan/io/validate_dims.hpp>
#include <stan/io/var_context.hpp>

#include <cmath>
#include <limits>
#include <map>
#include <optional>
#include <ostream>
#include <set>
#include <streambuf>
#include <string>
#include <utility>
#include <vector>

namespace stanr {

// R list -> stan::io::var_context adapter. Values are copied during
// construction, so Stan worker threads never access the R API.
//
// Tuple-typed values arrive as (nested) unnamed R lists and are flattened
// into the dotted per-leaf entries the generated Stan reader expects,
// guided by `declarations` (one block of `model$variables()`):
//   * tuple variable `x` is read slot-by-slot, never as a whole -- dotted
//     names `x.1`, `x.2.1`, ... one per leaf.
//   * array-of-tuple values are "blocked AoS": enclosing-array elements
//     enumerated column-major, each element's leaf payload contiguous.
//   * a tuple-slot complex leaf uses the windowed vals_c() layout stanc
//     2.39's reader indexes (see store_complex()).
class r_data_context : public stan::io::var_context {
 private:
  struct value_entry {
    std::vector<double> reals;
    std::vector<std::complex<double>> complexes;
    std::optional<std::vector<int>> ints;
    std::vector<size_t> dims;
  };

  // One node per variable, storing its name once.
  std::map<std::string, value_entry> values_;
  // Top-level names; flattened tuple leaves must not collide with them.
  std::set<std::string> reserved_names_;

  void add_value(const std::string& name, SEXP value);
  void store_numeric(const std::string& name, std::vector<double> values,
                     std::vector<size_t> dims);
  void store_complex(const std::string& name,
                     std::vector<std::complex<double>> values,
                     std::vector<size_t> dims, size_t enclosing_dims);
  void flatten_tuple(const std::string& name, SEXP value, Rcpp::List type_df,
                     int n_array_dims);
  void flatten_recurse(const std::string& name,
                       const std::vector<SEXP>& elements,
                       const std::vector<int>& array_sizes,
                       Rcpp::List type_df);
  void flatten_leaf(const std::string& name,
                    const std::vector<SEXP>& slot_values,
                    const std::string& kind,
                    const std::vector<int>& array_sizes);

 public:
  explicit r_data_context(Rcpp::List list, SEXP declarations = R_NilValue);

  bool contains_r(const std::string& name) const override;
  std::vector<double> vals_r(const std::string& name) const override;
  std::vector<std::complex<double>> vals_c(
      const std::string& name) const override;
  std::vector<size_t> dims_r(const std::string& name) const override;
  bool contains_i(const std::string& name) const override;
  std::vector<int> vals_i(const std::string& name) const override;
  std::vector<size_t> dims_i(const std::string& name) const override;
  void names_r(std::vector<std::string>& names) const override;
  void names_i(std::vector<std::string>& names) const override;
  void validate_dims(const std::string& stage, const std::string& name,
                     const std::string& base_type,
                     const std::vector<size_t>& dims_declared) const override;
};

}  // namespace stanr

#endif  // STANR_R_DATA_CONTEXT_HPP
