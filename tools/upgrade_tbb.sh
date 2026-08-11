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

rm -f "$SRC/tbb/itt_notify.cpp"
rm -rf "$SRC/tbb/tools_api"

# Upstream's include assumes its own nested src/tbbmalloc + src/tbb layout
# (".." lands on "src", then descends back into "tbb"); we vendor tbb/ and
# tbbmalloc/ as flat siblings under src/, so fix it to match every other
# cross-reference in this file (e.g. "../tbb/itt_notify.h").
sed -i.bak 's#\.\./src/tbb/environment\.h#../tbb/environment.h#' "$SRC/tbbmalloc/large_objects.cpp"
rm -f "$SRC/tbbmalloc/large_objects.cpp.bak"
