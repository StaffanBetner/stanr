#ifndef NEWSTAN_GET_ARG_HPP
#define NEWSTAN_GET_ARG_HPP


#include <Rcpp.h>

namespace newstan {
  inline double get_double(Rcpp::List args, const std::string& key, double default_val) {
    if (args.containsElementNamed(key.c_str()))
      return Rcpp::as<double>(args[key]);
    return default_val;
  }

  inline int get_int(Rcpp::List args, const std::string& key, int default_val) {
    if (args.containsElementNamed(key.c_str()))
      return Rcpp::as<int>(args[key]);
    return default_val;
  }

  inline unsigned int get_uint(Rcpp::List args, const std::string& key, unsigned int default_val) {
    if (args.containsElementNamed(key.c_str()))
      return Rcpp::as<unsigned int>(args[key]);
    return default_val;
  }

  inline std::string get_string(Rcpp::List args, const std::string& key, const std::string& default_val) {
    if (args.containsElementNamed(key.c_str()))
      return Rcpp::as<std::string>(args[key]);
    return default_val;
  }

  inline bool get_bool(Rcpp::List args, const std::string& key, bool default_val) {
    if (args.containsElementNamed(key.c_str()))
      return Rcpp::as<bool>(args[key]);
    return default_val;
  }
}

#endif
