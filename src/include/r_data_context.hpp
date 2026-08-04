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
class r_data_context : public stan::io::var_context {
 private:
  struct value_entry {
    std::vector<double> reals;
    std::vector<std::complex<double>> complexes;
    std::optional<std::vector<int>> ints;
    std::vector<size_t> dims;
  };

  // Keep all data associated with a variable in one node.  This stores its
  // name once instead of once per real, integer, and dimension map.
  std::map<std::string, value_entry> values_;

 public:
  explicit r_data_context(Rcpp::List list);

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
