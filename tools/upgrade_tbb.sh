#!/bin/sh
# Vendors oneTBB, following this package's convention for bundled libraries
# (see src/sundials, inst/include/{stan,walnutpie,...}): public headers go
# in inst/include (installed with the package), implementation sources go
# directly under src/ (compiled by src/Makevars, not installed). Only
# src/tbb and src/tbbmalloc are needed -- no cmake -- see src/Makevars.
set -e

ONETBB_VERSION=2022.0.0
ONETBB_DIR=oneTBB-$ONETBB_VERSION
ONETBB_TARBALL=v$ONETBB_VERSION.tar.gz
ONETBB_URL=https://github.com/oneapi-src/oneTBB/archive/refs/tags/$ONETBB_TARBALL

if [ ! -f "$ONETBB_TARBALL" ]; then
  wget $ONETBB_URL
fi
rm -rf "$ONETBB_DIR"
tar -xf "$ONETBB_TARBALL"

INST_INCLUDE=../inst/include
SRC=../src

rm -rf "$INST_INCLUDE/oneapi" "$INST_INCLUDE/tbb" "$SRC/tbb" "$SRC/tbbmalloc" "$SRC/tbbmalloc_proxy"

cp -Rf "$ONETBB_DIR/include/oneapi" "$INST_INCLUDE/"
cp -Rf "$ONETBB_DIR/include/tbb" "$INST_INCLUDE/"

cp -Rf "$ONETBB_DIR/src/tbb" "$SRC/"
cp -Rf "$ONETBB_DIR/src/tbbmalloc" "$SRC/"

# Customize.h includes this header just for its macros; nothing else in
# tbbmalloc_proxy is needed.
mkdir -p "$SRC/tbbmalloc_proxy"
cp -f "$ONETBB_DIR/src/tbbmalloc_proxy/proxy.h" "$SRC/tbbmalloc_proxy/"

# Only used by oneTBB's own CMake build.
rm -rf "$SRC/tbb/def" "$SRC/tbbmalloc/def"
rm -f "$SRC/tbb/tbb.rc" "$SRC/tbbmalloc/tbbmalloc.rc"
rm -f "$SRC/tbb/CMakeLists.txt" "$SRC/tbbmalloc/CMakeLists.txt"

# RTM/TSX speculative-lock fast path: src/Makevars never passes -mrtm (it
# trips R CMD check's non-portable-flags NOTE), so these are never compiled.
rm -f "$SRC/tbb/rtm_mutex.cpp" "$SRC/tbb/rtm_rw_mutex.cpp"

# WAITPKG (_tpause/_umwait) pause-loop fast path: __TBB_WAITPKG_INTRINSICS_PRESENT
# is gated only on compiler version, unlike __TBB_TSX_INTRINSICS_PRESENT above
# (gated on __RTM__, which is only predefined given -mrtm) -- so on a
# sufficiently new GCC/Clang it's "on" even though we never pass -mwaitpkg,
# and _tpause() (an always_inline intrinsic requiring that target feature)
# fails to inline into prolonged_pause() with a hard compile error. Force it
# off; prolonged_pause_impl() is the always-available fallback.
# Adds one open paren after the macro name and one matching close paren at
# the expression's end (`&& !__ANDROID__)` -> `&& !__ANDROID__))`) to wrap
# the whole thing in `(0 && (...))`.
sed -i.bak \
  -e 's/#define __TBB_WAITPKG_INTRINSICS_PRESENT (/#define __TBB_WAITPKG_INTRINSICS_PRESENT (0 \&\& (/' \
  -e 's/\&\& !__ANDROID__)/\&\& !__ANDROID__))/' \
  "$INST_INCLUDE/oneapi/tbb/detail/_config.h"
rm -f "$INST_INCLUDE/oneapi/tbb/detail/_config.h.bak"

rm -f "$SRC/tbb/itt_notify.cpp"
rm -rf "$SRC/tbb/tools_api"

# Upstream's include assumes its own nested src/tbbmalloc + src/tbb layout
# (".." lands on "src", then descends back into "tbb"); we vendor tbb/ and
# tbbmalloc/ as flat siblings under src/, so fix it to match every other
# cross-reference in this file (e.g. "../tbb/itt_notify.h").
sed -i.bak 's#\.\./src/tbb/environment\.h#../tbb/environment.h#' "$SRC/tbbmalloc/large_objects.cpp"
rm -f "$SRC/tbbmalloc/large_objects.cpp.bak"

# R CMD check flags non-portable diagnostic-suppression pragmas.
files_list=(
  "$SRC/tbb/co_context.h"
  "$SRC/tbb/concurrent_monitor.h"
  "$SRC/tbbmalloc/tbbmalloc_internal.h"
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
