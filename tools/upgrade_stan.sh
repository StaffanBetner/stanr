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
if [ ! -f "cmdstan-$CMDSTAN_VER.tar.gz" ]; then
  wget https://github.com/stan-dev/cmdstan/releases/download/v$CMDSTAN_VER/cmdstan-$CMDSTAN_VER.tar.gz
fi
rm -rf "$CMDSTAN_DIR"
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

rm -rf "$INC/boost"
mkdir -p "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/math "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/numeric "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/serialization "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/preprocessor "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/mpl "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/utility "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/type_traits "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/typeof "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/units "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/integer "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/fusion "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/range "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/iterator "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/concept "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/function_types "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/multi_array "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/random "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/optional "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/io "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/circular_buffer "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/*.hpp "$INC/boost"

cp -Rf "$MATH_SRC"/lib/boost_*/boost/lexical_cast "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/config "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/exception "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/assert "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/detail "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/core "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/container "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/move "$INC/boost"

# Trim filename for CRAN 100-character limit
mv "$INC/boost/math/special_functions/detail/hypergeometric_1F1_small_a_negative_b_by_ratio.hpp" \
  "$INC/boost/math/special_functions/detail/hypergeometric_1F1_small_a_neg_b_by_r.hpp"
mv "$INC/boost/numeric/odeint/stepper/generation/generation_controlled_adams_bashforth_moulton.hpp" \
  "$INC/boost/numeric/odeint/stepper/generation/generation_controlled_a_b_m.hpp"
mv "$INC/boost/numeric/odeint/stepper/generation/generation_runge_kutta_cash_karp54_classic.hpp" \
  "$INC/boost/numeric/odeint/stepper/generation/generation_runge_k_c_k_c.hpp"

# Update includes for renamed header
sed -i.bak \
  -e 's/hypergeometric_1F1_small_a_negative_b_by_ratio\.hpp/hypergeometric_1F1_small_a_neg_b_by_r.hpp/' \
  "$INC/boost/math/special_functions/hypergeometric_1F1.hpp"
rm -f "$INC/boost/math/special_functions/hypergeometric_1F1.hpp.bak"


sed -i.bak \
  -e 's/generation\/generation_controlled_adams_bashforth_moulton\.hpp/generation\/generation_controlled_a_b_m.hpp/' \
  "$INC/boost/numeric/odeint/stepper/generation.hpp"
rm -f "$INC/boost/numeric/odeint/stepper/generation.hpp.bak"

sed -i.bak \
  -e 's/generation\/generation_runge_kutta_cash_karp54_classic\.hpp/generation\/generation_runge_k_c_k_c.hpp/' \
  "$INC/boost/numeric/odeint/stepper/generation.hpp"
rm -f "$INC/boost/numeric/odeint/stepper/generation.hpp.bak"

rm -rf "$INC/Eigen"
rm -rf "$INC/unsupported"
cp -Rf "$MATH_SRC"/lib/eigen_*/Eigen "$INC/"
# Create the destination first so `cp -Rf` copies the `Eigen` directory
# *into* it (yielding $INC/unsupported/Eigen/...). Without it, cp flattens
# the source's contents directly into $INC/unsupported, breaking the
# `<unsupported/Eigen/...>` include paths Stan relies on.
mkdir -p "$INC/unsupported"
cp -Rf "$MATH_SRC"/lib/eigen_*/unsupported/Eigen "$INC/unsupported/"


files_list=(
  "$INC/CL/cl_platform.h"
  "$INC/Eigen/src/Core/util/DisableStupidWarnings.h"
  "$INC/boost/math/ccmath/isinf.hpp"
  "$INC/boost/container/allocator_traits.hpp"
  "$INC/boost/container/string.hpp"
  "$INC/boost/container/detail/config_begin.hpp"
  "$INC/boost/container/detail/flat_tree.hpp"
  "$INC/boost/container/detail/is_container.hpp"
  "$INC/boost/container/detail/is_contiguous_container.hpp"
  "$INC/boost/container/detail/node_alloc_holder.hpp"
  "$INC/boost/container/node_handle.hpp"
  "$INC/boost/container/small_vector.hpp"
  "$INC/boost/container/stable_vector.hpp"
  "$INC/boost/get_pointer.hpp"
  "$INC/boost/iterator/advance.hpp"
  "$INC/boost/move/algo/adaptive_merge.hpp"
  "$INC/boost/move/algo/adaptive_sort.hpp"
  "$INC/boost/move/algo/detail/adaptive_sort_merge.hpp"
  "$INC/boost/move/algo/detail/heap_sort.hpp"
  "$INC/boost/move/algo/detail/insertion_sort.hpp"
  "$INC/boost/move/algo/detail/merge_sort.hpp"
  "$INC/boost/move/algo/detail/merge.hpp"
  "$INC/boost/move/algo/detail/pdqsort.hpp"
  "$INC/boost/move/algo/detail/search.hpp"
  "$INC/boost/move/algo/detail/set_difference.hpp"
  "$INC/boost/move/detail/std_ns_begin.hpp"
  "$INC/boost/mpl/assert.hpp"
  "$INC/boost/random/detail/disable_warnings.hpp"
  "$INC/boost/range/adaptor/indexed.hpp"
  "$INC/boost/type_traits/detail/has_prefix_operator.hpp"
  "$INC/boost/type_traits/has_logical_not.hpp"
)

for file in "${files_list[@]}"; do
  if [ -f "$file" ]; then
    sed -i.bak \
      -e '/#pragma clang diagnostic/d' \
      -e '/#pragma GCC diagnostic/d' \
      -e '/#pragma warning( *push *)/d' \
      -e '/#pragma warning( *disable *:/d' \
      -e '/#pragma warning( *pop *)/d' \
      -e '/#pragma warning push/d' \
      -e '/#pragma warning disable/d' \
      "$file"
    rm -f "$file.bak"
  fi
done

echo "Done. Vendored stan and math headers into inst/include from CmdStan $CMDSTAN_VER."
