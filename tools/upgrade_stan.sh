#!/bin/sh
# Vendors the headers provided by the `stan` and `math` submodules directly
# into inst/include, using the files bundled in a CmdStan release tarball.
#
# The CmdStan release bundles:
#   cmdstan-<VER>/stan/                  -> the `stan` submodule (stan-dev/stan)
#   cmdstan-<VER>/stan/lib/stan_math/    -> the `math` submodule (stan-dev/math)
#
# This replicates the header copies that src/Makevars' `package:` target
# currently performs at install time, so that step can be removed from
# Makevars and the headers are vendored into the package instead.
set -e

CMDSTAN_VER="2.39.0"
CMDSTAN_DIR="cmdstan-$CMDSTAN_VER"
STAN_SRC="$CMDSTAN_DIR/stan"
MATH_SRC="$CMDSTAN_DIR/stan/lib/stan_math"
INC=../inst/include

# --- 1. Download and extract the CmdStan release ---------------------------
wget https://github.com/stan-dev/cmdstan/releases/download/v$CMDSTAN_VER/cmdstan-$CMDSTAN_VER.tar.gz
tar -xf cmdstan-$CMDSTAN_VER.tar.gz

# --- 2. Vendor the `stan` headers ------------------------------------------
# Makevars: cp -Rf stan/src/stan ../inst/include/stan
rm -rf "$INC/stan"
cp -Rf "$STAN_SRC/src/stan" "$INC/stan"

# --- 3. Vendor the `math` headers (merged into inst/include/stan) ----------
# Makevars: cp -Rf math/stan/. ../inst/include/stan
cp -Rf "$MATH_SRC/stan/." "$INC/stan"

# --- 4. Vendor the OpenCL headers ------------------------------------------
# Makevars: cp -Rf math/lib/opencl_*/CL ../inst/include/
rm -rf "$INC/CL"
cp -Rf "$MATH_SRC"/lib/opencl_*/CL "$INC/"

# --- 5. Vendor the Sundials headers ----------------------------------------
# Makevars: cp -Rf math/lib/sundials_*/include/* ../inst/include/
cp -Rf "$MATH_SRC"/lib/sundials_*/include/* "$INC/"

# --- 6. Vendor the Sundials C sources --------------------------------------
# Makevars: cp -Rf math/lib/sundials_*/src/sundials .
rm -rf ../src/sundials
cp -Rf "$MATH_SRC"/lib/sundials_*/src/sundials ../src/sundials

# --- 7. Strip diagnostic-suppression pragmas from the bundled OpenCL header -
# Mirrors the `cleanup` script run by Makevars' `package:` target, so the
# vendored header is already clean for R CMD check.
CL_HEADER="$INC/CL/cl_platform.h"
if [ -f "$CL_HEADER" ]; then
  sed -i.bak \
    -e '/#pragma clang diagnostic/d' \
    -e '/#pragma warning( *push *)/d' \
    -e '/#pragma warning( *disable *:/d' \
    -e '/#pragma warning( *pop *)/d' \
    "$CL_HEADER"
  rm -f "$CL_HEADER.bak"
fi

rm -rf "$INC/boost"
mkdir -p "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/math "$INC/boost"

# Trim filename for CRAN 100-character limit
mv "$INC/boost/math/special_functions/detail/hypergeometric_1F1_small_a_negative_b_by_ratio.hpp" \
  "$INC/boost/math/special_functions/detail/hypergeometric_1F1_small_a_neg_b_by_r.hpp"

# Update includes for renamed header
sed -i.bak \
  -e 's/hypergeometric_1F1_small_a_negative_b_by_ratio\.hpp/hypergeometric_1F1_small_a_neg_b_by_r.hpp/' \
  "$INC/boost/math/special_functions/hypergeometric_1F1.hpp"
rm -f "$INC/boost/math/special_functions/hypergeometric_1F1.hpp.bak"

rm -rf "$INC/Eigen"
cp -Rf "$MATH_SRC"/lib/eigen_*/Eigen "$INC/"

# --- 8. Strip diagnostic pragmas from vendored headers ---------------------
# R CMD check's pragma scan is textual, not compiler-aware, and flags
# "#pragma clang/warning/GCC diagnostic ..." wherever it appears regardless
# of the surrounding #ifdef guard. Strip them from the vendored headers.
EIGEN_WARN="$INC/Eigen/src/Core/util/DisableStupidWarnings.h"
if [ -f "$EIGEN_WARN" ]; then
  sed -i.bak \
    -e '/#pragma clang diagnostic/d' \
    -e '/#pragma GCC diagnostic/d' \
    -e '/#pragma warning( *push *)/d' \
    -e '/#pragma warning( *disable *:/d' \
    -e '/#pragma warning( *pop *)/d' \
    -e '/#pragma warning push/d' \
    -e '/#pragma warning disable/d' \
    "$EIGEN_WARN"
  rm -f "$EIGEN_WARN.bak"
fi

BOOST_ISINF="$INC/boost/math/ccmath/isinf.hpp"
if [ -f "$BOOST_ISINF" ]; then
  sed -i.bak \
    -e '/#pragma clang diagnostic/d' \
    "$BOOST_ISINF"
  rm -f "$BOOST_ISINF.bak"
fi

echo "Done. Vendored stan and math headers into inst/include from CmdStan $CMDSTAN_VER."
