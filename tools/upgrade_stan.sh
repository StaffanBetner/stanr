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
# stan-math's own make/libraries builds CVODES/IDAS/KINSOL (ode_*, dae,
# algebra_solver) from sibling directories under src/, not just src/sundials
# -- see $(SUNDIALS_CVODES)/$(SUNDIALS_IDAS)/$(SUNDIALS_KINSOL)/
# $(SUNDIALS_NVECSERIAL) there. Vendor those siblings the same way, so
# src/Makevars can compile them too.
SUNDIALS_SRC="$MATH_SRC"/lib/sundials_*/src
rm -rf ../src/sundials ../src/cvodes ../src/idas ../src/kinsol ../src/nvector \
  ../src/sunmatrix ../src/sunlinsol ../src/sunnonlinsol
cp -Rf "$MATH_SRC"/lib/sundials_*/src/sundials ../src/sundials
cp -Rf "$MATH_SRC"/lib/sundials_*/src/cvodes ../src/cvodes
cp -Rf "$MATH_SRC"/lib/sundials_*/src/idas ../src/idas
cp -Rf "$MATH_SRC"/lib/sundials_*/src/kinsol ../src/kinsol
cp -Rf "$MATH_SRC"/lib/sundials_*/src/nvector ../src/nvector
cp -Rf "$MATH_SRC"/lib/sundials_*/src/sunmatrix ../src/sunmatrix
cp -Rf "$MATH_SRC"/lib/sundials_*/src/sunlinsol ../src/sunlinsol
cp -Rf "$MATH_SRC"/lib/sundials_*/src/sunnonlinsol ../src/sunnonlinsol

# --- 6b. Shim sundials' raw stdio ------------------------------------------
# R CMD check flags compiled code that calls stdio entry points which write
# to stdout/stderr instead of R's console, or that format via unbounded
# [v]sprintf. stan_sundials_printf_override.hpp already neuters the optional
# STAN_SUNDIALS_FPRINTF diagnostic calls (no-ops unless WITH_SUNDIAL_PRINTF is
# defined, which it never is here), but it doesn't reach two other things:
# each solver's errfp/infofp default to the real stdout/stderr streams
# (read only by the already-neutered fprintf, so nothing observable changes
# by defaulting to NULL instead -- every read site null-checks first), and
# each solver's *ProcessError composes its message via unbounded vsprintf
# into a fixed buffer before doing anything else with it, which is a real
# overflow risk independent of where the message ends up. vsnprintf is the
# same behavior, bounded, and not one of the flagged symbols.
python3 - "../src/idas/idas.c" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old = "  IDA_mem->ida_errfp          = stderr;\n"
new = "  IDA_mem->ida_errfp          = NULL;\n"
assert old in text, "ida_errfp default not found -- idas.c changed upstream"
text = text.replace(old, new, 1)

old = "  vsprintf(msg, msgfmt, ap);\n"
new = "  vsnprintf(msg, sizeof msg, msgfmt, ap);\n"
assert old in text, "IDAProcessError vsprintf not found -- idas.c changed upstream"
text = text.replace(old, new, 1)

open(path, "w").write(text)
EOF

python3 - "../src/cvodes/cvodes.c" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old = "  cv_mem->cv_errfp            = stderr;\n"
new = "  cv_mem->cv_errfp            = NULL;\n"
assert old in text, "cv_errfp default not found -- cvodes.c changed upstream"
text = text.replace(old, new, 1)

old = "  vsprintf(msg, msgfmt, ap);\n"
new = "  vsnprintf(msg, sizeof msg, msgfmt, ap);\n"
assert old in text, "cvProcessError vsprintf not found -- cvodes.c changed upstream"
text = text.replace(old, new, 1)

open(path, "w").write(text)
EOF

# kinsol has TWO identical `vsprintf(msg, msgfmt, ap);` statements
# (KINPrintInfo and KINProcessError) at different indentation. A plain
# string replace risks a partial match of one inside the other's leading
# whitespace, so match the whole line via regex instead and assert the count.
python3 - "../src/kinsol/kinsol.c" << 'EOF'
import re
import sys

path = sys.argv[1]
text = open(path).read()

old = "  kin_mem->kin_errfp            = stderr;\n"
new = "  kin_mem->kin_errfp            = NULL;\n"
assert old in text, "kin_errfp default not found -- kinsol.c changed upstream"
text = text.replace(old, new, 1)

old = "  kin_mem->kin_infofp           = stdout;\n"
new = "  kin_mem->kin_infofp           = NULL;\n"
assert old in text, "kin_infofp default not found -- kinsol.c changed upstream"
text = text.replace(old, new, 1)

text, n = re.subn(
    r"^(\s*)vsprintf\(msg, msgfmt, ap\);$",
    r"\1vsnprintf(msg, sizeof msg, msgfmt, ap);",
    text,
    flags=re.MULTILINE,
)
assert n == 2, (
    f"expected 2 vsprintf(msg, msgfmt, ap) call sites in kinsol.c, found {n} "
    "-- changed upstream"
)

open(path, "w").write(text)
EOF

python3 - "../src/sunnonlinsol/newton/sunnonlinsol_newton.c" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old = "  content->info_file   = stdout;\n"
new = "  content->info_file   = NULL;\n"
assert old in text, "info_file default not found -- sunnonlinsol_newton.c changed upstream"
text = text.replace(old, new, 1)

open(path, "w").write(text)
EOF

python3 - "../src/nvector/serial/nvector_serial.c" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old = "  N_VPrintFile_Serial(x, stdout);\n"
new = "  N_VPrintFile_Serial(x, NULL);\n"
assert old in text, "N_VPrint_Serial call not found -- nvector_serial.c changed upstream"
text = text.replace(old, new, 1)

open(path, "w").write(text)
EOF

# --- 6c. Shim sundials' remaining raw sprintf calls -------------------------
# Beyond the vsprintf/errfp/infofp cases in 6b, each solver's return-code-
# to-name helper (e.g. CVodeGetReturnFlagName) and its default error/info
# handlers format known-short string literals into a buffer with plain
# sprintf. There's no overflow risk -- every literal fits the buffer it's
# copied into -- but the fortified libc still lowers these to the
# ___sprintf_chk symbol R CMD check flags, same as unbounded vsprintf.
# Rewrite each to snprintf with that buffer's known size.
patch_sprintf() {
  # $1 = file, $2 = destination buffer, $3 = snprintf size arg,
  # $4 = expected number of call sites (guards against upstream drift)
  python3 - "$1" "$2" "$3" "$4" << 'EOF'
import re
import sys

path, var, size, expected = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
text = open(path).read()

# \b excludes vsprintf(...), already handled in 6b.
pattern = re.compile(r"\bsprintf\(" + re.escape(var) + r",\s*")
text, n = pattern.subn(f"snprintf({var}, {size}, ", text)
assert n == expected, (
    f"expected {expected} sprintf({var}, ...) call sites in {path}, found {n} "
    "-- changed upstream"
)

open(path, "w").write(text)
EOF
}

# `name` is a malloc'd pointer (size not recoverable via sizeof), sized
# per-file at the call site below; the rest are fixed-size local arrays.
patch_sprintf "../src/idas/idas.c" "err_type" "sizeof err_type" 2
patch_sprintf "../src/idas/idas_io.c" "name" 24 45
patch_sprintf "../src/idas/idas_ls.c" "name" 30 11
patch_sprintf "../src/kinsol/kinsol.c" "err_type" "sizeof err_type" 2
patch_sprintf "../src/kinsol/kinsol.c" "retstr" "sizeof retstr" 11
patch_sprintf "../src/kinsol/kinsol.c" "msg1" "sizeof msg1" 1
patch_sprintf "../src/kinsol/kinsol.c" "msg" "sizeof msg" 1
patch_sprintf "../src/kinsol/kinsol_io.c" "name" 24 17
patch_sprintf "../src/kinsol/kinsol_ls.c" "name" 30 10
patch_sprintf "../src/cvodes/cvodes.c" "err_type" "sizeof err_type" 2
patch_sprintf "../src/cvodes/cvodes_io.c" "name" 24 43
patch_sprintf "../src/cvodes/cvodes_diag.c" "name" 30 10
patch_sprintf "../src/cvodes/cvodes_ls.c" "name" 30 13

# --- 7. TBB ------------------------------------------------------------
# TBB is not vendored from the CmdStan/math bundle -- see tools/upgrade_tbb.sh,
# which vendors a standalone oneTBB release (headers into inst/include,
# sources into src/) independently.

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
cp -Rf "$MATH_SRC"/lib/boost_*/boost/accumulators "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/parameter "$INC/boost"
cp -Rf "$MATH_SRC"/lib/boost_*/boost/mp11 "$INC/boost"
# odeint and boost::math each probe __has_include(<boost/predef/other/endian.h>)
# to decide whether to fall back to a "standalone" build (see their
# tools/is_standalone.hpp). Without this vendored, both silently switch to
# raw assert() instead of the overridable BOOST_ASSERT, so R CMD check's
# compiled-code scan flags ___assert_rtn no matter what PKG_CPPFLAGS says.
cp -Rf "$MATH_SRC"/lib/boost_*/boost/predef "$INC/boost"
cp -f "$MATH_SRC"/lib/boost_*/boost/predef.h "$INC/boost/"
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


for file in \
  "$INC/CL/cl_platform.h" \
  "$INC/Eigen/src/Core/util/DisableStupidWarnings.h" \
  "$INC/boost/math/ccmath/isinf.hpp" \
  "$INC/boost/container/allocator_traits.hpp" \
  "$INC/boost/container/string.hpp" \
  "$INC/boost/container/detail/config_begin.hpp" \
  "$INC/boost/container/detail/flat_tree.hpp" \
  "$INC/boost/container/detail/is_container.hpp" \
  "$INC/boost/container/detail/is_contiguous_container.hpp" \
  "$INC/boost/container/detail/node_alloc_holder.hpp" \
  "$INC/boost/container/node_handle.hpp" \
  "$INC/boost/container/small_vector.hpp" \
  "$INC/boost/container/stable_vector.hpp" \
  "$INC/boost/get_pointer.hpp" \
  "$INC/boost/iterator/advance.hpp" \
  "$INC/boost/move/algo/adaptive_merge.hpp" \
  "$INC/boost/move/algo/adaptive_sort.hpp" \
  "$INC/boost/move/algo/detail/adaptive_sort_merge.hpp" \
  "$INC/boost/move/algo/detail/heap_sort.hpp" \
  "$INC/boost/move/algo/detail/insertion_sort.hpp" \
  "$INC/boost/move/algo/detail/merge_sort.hpp" \
  "$INC/boost/move/algo/detail/merge.hpp" \
  "$INC/boost/move/algo/detail/pdqsort.hpp" \
  "$INC/boost/move/algo/detail/search.hpp" \
  "$INC/boost/move/algo/detail/set_difference.hpp" \
  "$INC/boost/move/detail/std_ns_begin.hpp" \
  "$INC/boost/mpl/assert.hpp" \
  "$INC/boost/random/detail/disable_warnings.hpp" \
  "$INC/boost/range/adaptor/indexed.hpp" \
  "$INC/boost/type_traits/detail/has_prefix_operator.hpp" \
  "$INC/boost/type_traits/has_logical_not.hpp"
do
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
