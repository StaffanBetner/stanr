#ifndef NEWSTAN_RCPP_TUPLE_INTEROP_HPP
#define NEWSTAN_RCPP_TUPLE_INTEROP_HPP

// R list <-> std::tuple marshalling for exposed Stan functions.
// Adapted from cmdstanr's inst/include/rcpp_tuple_interop.hpp with two
// changes: C++17 std::apply replaces stan::math::apply (no Stan dependency),
// and a wrap() overload for tuple-containing std::vector nestings -- Rcpp's
// generic dispatcher cannot find the tuple wrap for nested elements
// (std::tuple has no ADL association with namespace Rcpp).

#include <Rcpp.h>
#include <tuple>
#include <vector>
#include <type_traits>
#include <utility>
#include <cstddef>

namespace newstan {
template <typename T> struct contains_tuple : std::false_type {};
template <typename... T>
struct contains_tuple<std::tuple<T...>> : std::true_type {};
template <typename T>
struct contains_tuple<std::vector<T>> : contains_tuple<T> {};
}  // namespace newstan

namespace Rcpp {
// Declarations first: each definition must see both so that
// tuple-inside-array-inside-tuple nestings resolve.
template <typename... T> SEXP wrap(const std::tuple<T...>& x);
template <typename T, typename std::enable_if_t<
    newstan::contains_tuple<T>::value, bool> = true>
SEXP wrap(const std::vector<T>& x);

namespace traits {
template <typename... T>
class Exporter<std::tuple<T...>> {
  Rcpp::List list_x;
  template <std::size_t... I>
  auto get_impl(std::index_sequence<I...>) {
    return std::make_tuple(Rcpp::as<T>(list_x[I].get())...);
  }
 public:
  Exporter(SEXP x) : list_x(x) {}
  std::tuple<T...> get() { return get_impl(std::index_sequence_for<T...>{}); }
};
// No Exporter needed for std::vector<tuple>: Rcpp's RangeExporter already
// imports generic element types via per-element as<T>.
}  // namespace traits

template <typename... T>
SEXP wrap(const std::tuple<T...>& x) {
  return std::apply(
      [](const auto&... args) { return Rcpp::List::create(Rcpp::wrap(args)...); },
      x);
}

template <typename T, typename std::enable_if_t<
    newstan::contains_tuple<T>::value, bool>>
SEXP wrap(const std::vector<T>& x) {
  Rcpp::List out(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) out[i] = Rcpp::wrap(x[i]);
  return out;
}
}  // namespace Rcpp

#endif  // NEWSTAN_RCPP_TUPLE_INTEROP_HPP
