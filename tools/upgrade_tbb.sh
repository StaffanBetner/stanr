#!/bin/sh
# Vendors oneTBB into inst/tbb. src/Makevars compiles it directly (no cmake),
# so only src/tbb and src/tbbmalloc are needed -- see src/Makevars.
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

INST_TBB=../inst/tbb
rm -rf "$INST_TBB"
mkdir -p "$INST_TBB"

cp -Rf "$ONETBB_DIR/include" "$INST_TBB/"
mkdir -p "$INST_TBB/src"
cp -Rf "$ONETBB_DIR/src/tbb" "$INST_TBB/src/"
cp -Rf "$ONETBB_DIR/src/tbbmalloc" "$INST_TBB/src/"
cp -f "$ONETBB_DIR/LICENSE.txt" "$INST_TBB/"

# Customize.h includes this header just for its macros; nothing else in
# tbbmalloc_proxy is needed.
mkdir -p "$INST_TBB/src/tbbmalloc_proxy"
cp -f "$ONETBB_DIR/src/tbbmalloc_proxy/proxy.h" "$INST_TBB/src/tbbmalloc_proxy/"

# Only used by oneTBB's own CMake build.
rm -rf "$INST_TBB/src/tbb/def" "$INST_TBB/src/tbbmalloc/def"
rm -f "$INST_TBB/src/tbb/tbb.rc" "$INST_TBB/src/tbbmalloc/tbbmalloc.rc"
