#!/bin/sh
# Vendors stanli (github.com/seantalts/stanli), following this package's
# convention for bundled libraries 
set -e

STANLI_REF="422fe79"
STANLI_TARBALL="stanli-$STANLI_REF.tar.gz"
STANLI_URL="https://github.com/seantalts/stanli/archive/$STANLI_REF.tar.gz"

if [ ! -f "$STANLI_TARBALL" ]; then
  curl -sSL -o "$STANLI_TARBALL" "$STANLI_URL"
fi
tar -xf "$STANLI_TARBALL"
STANLI_DIR=$(find . -maxdepth 1 -type d -name 'stanli-*')

SRC=../src/stanli

rm -rf "$SRC"
mkdir -p "$SRC/runtime"

cp -f "$STANLI_DIR/LICENSE" "$SRC/"

cp -Rf "$STANLI_DIR/runtime/include" "$SRC/runtime/"
cp -Rf "$STANLI_DIR/runtime/src" "$SRC/runtime/"
cp -Rf "$STANLI_DIR/runtime/kernels" "$SRC/runtime/"
rm -f "$SRC/runtime/include/stanli/README.md"

rm -f "$SRC/runtime/src/capi.cpp" \
      "$SRC/runtime/src/bridgestan_abi.cpp" \
      "$SRC/runtime/src/stanc_embed_c.cpp" \
      "$SRC/runtime/src/nuts.cpp" \
      "$SRC/runtime/src/estimate.cpp" \
      "$SRC/runtime/src/diagnose.cpp" \
      "$SRC/runtime/src/walnuts.cpp" \
      "$SRC/runtime/src/data.cpp" \
      "$SRC/runtime/src/README.md" \
      "$SRC/runtime/src/OPTIMIZATIONS.md"
rm -f "$SRC/runtime/include/stanli/capi.h" \
      "$SRC/runtime/include/stanli/bridgestan_internal.hpp" \
      "$SRC/runtime/include/stanli/nuts.hpp" \
      "$SRC/runtime/include/stanli/estimate.hpp" \
      "$SRC/runtime/include/stanli/diagnose.hpp" \
      "$SRC/runtime/include/stanli/walnuts.hpp" \
      "$SRC/runtime/include/stanli/model_adapter.hpp"

# stanli::DataMap::set_int_array() only ever built a 1-D entry
python3 - "$SRC/runtime/include/stanli/data.hpp" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old_sig = "  void set_int_array(const std::string& name, std::vector<int> v) {\n"
new_sig = (
    "  void set_int_array(const std::string& name, std::vector<int> v,\n"
    "                     std::vector<int64_t> dims = {}) {\n"
)
assert old_sig in text, "set_int_array signature not found -- data.hpp changed upstream"
text = text.replace(old_sig, new_sig, 1)

old_dims = "    e.dims = {static_cast<int64_t>(v.size())};\n"
new_dims = (
    "    e.dims = dims.empty() ? std::vector<int64_t>{static_cast<int64_t>(v.size())}\n"
    "                          : std::move(dims);\n"
)
assert old_dims in text, "set_int_array dims assignment not found -- data.hpp changed upstream"
text = text.replace(old_dims, new_dims, 1)

old_from_json = (
    "  static DataMap from_json_file(const std::string& path);\n"
    "  static DataMap from_json(const std::string& text);\n"
    "\n"
)
assert old_from_json in text, "from_json declarations not found -- data.hpp changed upstream"
text = text.replace(old_from_json, "", 1)

open(path, "w").write(text)
EOF
