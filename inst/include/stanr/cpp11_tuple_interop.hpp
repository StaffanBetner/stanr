#ifndef STANR_CPP11_TUPLE_INTEROP_HPP
#define STANR_CPP11_TUPLE_INTEROP_HPP

#include <cpp11.hpp>
#include <stanr/cpp11_eigen_interop.hpp>
#include <stanr/r_vector_copy.hpp>
#include <complex>
#include <cstddef>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

// R list <-> std::tuple / Eigen / nested std::vector marshalling for exposed
// Stan functions. `stanr::as_cpp<T>`/`stanr::as_sexp` is what generated
// wrappers call for every argument/return type. A stanr-namespaced entry
// point is needed rather than adding more cpp11::as_cpp/as_sexp overloads
// directly: cpp11's own generic as_cpp<Container>/as_sexp(Container)
// (r_vector.hpp, as.hpp) already match any std::vector<T> (T not a string)
// or Eigen type (Eigen exposes `value_type` for STL compatibility), so a
// same-shaped overload in namespace cpp11 for e.g. std::vector<std::tuple<>>
// or Eigen::Matrix<...> would be ambiguous with it. Only plain scalars fall
// through to cpp11::as_cpp/as_sexp.

namespace stanr {

template <typename T>
struct is_tuple : std::false_type {};
template <typename... T>
struct is_tuple<std::tuple<T...>> : std::true_type {};

template <typename T>
struct is_std_vector : std::false_type {};
template <typename T>
struct is_std_vector<std::vector<T>> : std::true_type {};

template <typename T>
T as_cpp(SEXP from) {
  if constexpr (is_eigen_matrix_type<T>::value) {
    return eigen_matrix_as_cpp<T>(from);
  } else if constexpr (is_eigen_map_matrix_type<T>::value) {
    return eigen_map_as_cpp<T>(from);
  } else if constexpr (std::is_same_v<T, std::complex<double>>) {
    Rcomplex z = COMPLEX_ELT(from, 0);
    return T(z.r, z.i);
  } else if constexpr (is_tuple<T>::value) {
    cpp11::list x(from);
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return T(as_cpp<std::tuple_element_t<I, T>>(x[I])...);
    }(std::make_index_sequence<std::tuple_size_v<T>>{});
  } else if constexpr (is_std_vector<T>::value) {
    using Elem = typename T::value_type;
    if constexpr (std::is_same_v<Elem, int>) {
      cpp11::integers x(from);
      return internal::copy_integer_values(x);
    } else if constexpr (std::is_same_v<Elem, double>) {
      cpp11::doubles x(from);
      return internal::copy_real_values(x);
    } else if constexpr (std::is_same_v<Elem, std::string>) {
      return cpp11::as_cpp<T>(from);
    } else if constexpr (std::is_same_v<Elem, std::complex<double>>) {
      R_xlen_t n = Rf_xlength(from);
      T out(n);
      for (R_xlen_t i = 0; i < n; ++i) {
        Rcomplex z = COMPLEX_ELT(from, i);
        out[i] = Elem(z.r, z.i);
      }
      return out;
    } else {
      cpp11::list x(from);
      T out;
      out.reserve(x.size());
      for (auto elt : x) out.push_back(as_cpp<Elem>(elt));
      return out;
    }
  } else {
    return cpp11::as_cpp<T>(from);
  }
}

template <typename T>
SEXP as_sexp(const T& x) {
  if constexpr (stan::is_eigen<T>::value) {
    return eigen_as_sexp(x);
  } else if constexpr (std::is_same_v<T, std::complex<double>>) {
    cpp11::sexp out = cpp11::safe[Rf_allocVector](CPLXSXP, 1);
    COMPLEX(out.data())[0] = Rcomplex{x.real(), x.imag()};
    return out;
  } else if constexpr (is_tuple<T>::value) {
    return std::apply(
        [](const auto&... args) { return cpp11::writable::list({as_sexp(args)...}); },
        x);
  } else if constexpr (is_std_vector<T>::value) {
    using Elem = typename T::value_type;
    if constexpr (std::is_same_v<Elem, int> || std::is_same_v<Elem, double>
                  || std::is_same_v<Elem, std::string>) {
      return cpp11::as_sexp(x);
    } else if constexpr (std::is_same_v<Elem, std::complex<double>>) {
      cpp11::sexp out = cpp11::safe[Rf_allocVector](CPLXSXP, x.size());
      Rcomplex* p = COMPLEX(out.data());
      for (std::size_t i = 0; i < x.size(); ++i) {
        p[i] = Rcomplex{x[i].real(), x[i].imag()};
      }
      return out;
    } else {
      cpp11::writable::list out(x.size());
      for (std::size_t i = 0; i < x.size(); ++i) out[i] = as_sexp(x[i]);
      return out;
    }
  } else {
    return cpp11::as_sexp(x);
  }
}

}  // namespace stanr

#endif  // STANR_CPP11_TUPLE_INTEROP_HPP
