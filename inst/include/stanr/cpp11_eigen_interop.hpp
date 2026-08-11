#ifndef STANR_CPP11_EIGEN_INTEROP_HPP
#define STANR_CPP11_EIGEN_INTEROP_HPP

#include <stan/math/prim/fun/to_array_1d.hpp>
#include <cpp11.hpp>
#include <complex>

// Eigen <-> SEXP marshalling, called from stanr::as_cpp/as_sexp
// (cpp11_tuple_interop.hpp). Not implemented as cpp11::as_cpp/as_sexp
// overloads: cpp11's own generic as_cpp<Container>/as_sexp(Container)
// (r_vector.hpp, as.hpp) already match any Eigen type too (Eigen exposes
// `value_type` for STL compatibility), so a same-shaped overload here would
// be ambiguous with it -- see cpp11_tuple_interop.hpp's file comment for the
// same issue with std::vector<tuple>.

namespace stanr {

template <typename T>
struct is_eigen_matrix_type : std::false_type {};
template <typename S, int R, int C>
struct is_eigen_matrix_type<Eigen::Matrix<S, R, C>> : std::true_type {};

template <typename T>
struct is_eigen_map_matrix_type : std::false_type {};
template <typename S, int R, int C>
struct is_eigen_map_matrix_type<Eigen::Map<Eigen::Matrix<S, R, C>>> : std::true_type {};

template <typename T>
T eigen_matrix_as_cpp(SEXP object) {
  using Scalar = typename T::Scalar;
  constexpr int R = T::RowsAtCompileTime;
  constexpr int C = T::ColsAtCompileTime;

  auto elt = [&](R_xlen_t i) -> Scalar {
    if constexpr (std::is_same_v<Scalar, std::complex<double>>) {
      Rcomplex z = COMPLEX_ELT(object, i);
      return Scalar(z.r, z.i);
    } else {
      return REAL_ELT(object, i);
    }
  };

  if constexpr (R == 1) {
    Eigen::Matrix<Scalar, 1, C> result(Rf_xlength(object));
    for (R_xlen_t i = 0; i < result.size(); ++i) result(i) = elt(i);
    return result;
  } else if constexpr (C == 1) {
    Eigen::Matrix<Scalar, R, 1> result(Rf_xlength(object));
    for (R_xlen_t i = 0; i < result.size(); ++i) result(i) = elt(i);
    return result;
  } else {
    cpp11::sexp dims(Rf_getAttrib(object, R_DimSymbol));
    if (dims == R_NilValue || Rf_xlength(dims) != 2) {
      cpp11::stop("Not a matrix.");
    }
    const int* d = INTEGER(dims);
    Eigen::Matrix<Scalar, R, C> result(d[0], d[1]);
    for (R_xlen_t i = 0; i < result.size(); ++i) result.data()[i] = elt(i);
    return result;
  }
}

// Zero-copy map over the R vector/matrix data. Only double is needed --
// run_standalone_gqs.hpp's `draws` argument is the sole caller.
template <typename T>
T eigen_map_as_cpp(SEXP object) {
  static_assert(std::is_same_v<typename T::Scalar, double>,
                "Only double Eigen::Map is supported.");
  constexpr int R = T::RowsAtCompileTime;
  constexpr int C = T::ColsAtCompileTime;

  if (TYPEOF(object) != REALSXP) {
    throw std::invalid_argument("Wrong R type for mapped Eigen object");
  }

  if constexpr (R == 1 || C == 1) {
    return T(REAL(object), Rf_xlength(object));
  } else {
    int nrow = Rf_xlength(object);
    int ncol = 1;
    if (Rf_isMatrix(object)) {
      const int* dims = INTEGER(Rf_getAttrib(object, R_DimSymbol));
      nrow = dims[0];
      ncol = dims[1];
    }
    return T(REAL(object), nrow, ncol);
  }
}

template <typename T>
SEXP eigen_as_sexp(const T& x) {
  using Scalar = stan::scalar_type_t<T>;
  const auto flat = stan::math::to_array_1d(x);

  cpp11::sexp result;
  if constexpr (std::is_same_v<Scalar, std::complex<double>>) {
    result = cpp11::safe[Rf_allocVector](CPLXSXP, flat.size());
    Rcomplex* p = COMPLEX(result.data());
    for (std::size_t i = 0; i < flat.size(); ++i) {
      p[i] = Rcomplex{flat[i].real(), flat[i].imag()};
    }
  } else {
    result = cpp11::as_sexp(flat);
  }

  if (!stan::is_eigen_col_vector<T>::value) {
    Rf_setAttrib(result.data(), R_DimSymbol,
                 cpp11::as_sexp(std::vector<int>{static_cast<int>(x.rows()),
                                                 static_cast<int>(x.cols())}));
  }
  return result;
}

}  // namespace stanr

#endif
