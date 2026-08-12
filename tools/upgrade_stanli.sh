#!/bin/sh
# Vendors stanli (github.com/seantalts/stanli), following this package's
# convention for bundled libraries 
set -e

STANLI_REF="88621f1"
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

# R CMD check flags raw stdio in compiled code (calls that write to
# stdout/stderr instead of R's console, or that use unbounded formatting).
# None of what follows is reachable through stanr: the STANLI_DEBUG_*
# traces are gated behind env vars stanr never sets, and profile_report()
# is never called -- so these are patched out/around rather than left to
# trip the check.

# print()/reject() with no sink installed fell back to raw stdout. stanr
# does not install a sink (chains run across TBB worker threads, where
# calling back into R's console API would not be safe), so drop the
# message instead of writing around R's console.
python3 - "$SRC/runtime/src/message_sink.cpp" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old_include = "#include <cstdio>\n"
assert old_include in text, "cstdio include not found -- message_sink.cpp changed upstream"
text = text.replace(old_include, "", 1)

old_body = """  if (sink()) {
    sink()(text.data(), text.size());
    return;
  }
  // The default, and what both paths did before there was a sink: the
  // line and its newline to stdout. One fwrite per call rather than two,
  // so a line cannot be split by another thread's output.
  std::string line = text;
  line += '\\n';
  std::fwrite(line.data(), 1, line.size(), stdout);
}
"""
new_body = """  if (sink()) sink()(text.data(), text.size());
  // No sink installed: stanr does not call set_message_sink() (yet), and
  // the default fallback wrote to the process's raw stdout, which R CMD
  // check flags. Drop the message rather than write around R's console.
}
"""
assert old_body in text, "emit_message body not found -- message_sink.cpp changed upstream"
text = text.replace(old_body, new_body, 1)

open(path, "w").write(text)
EOF

# STANLI_DEBUG_ISLAND traces to stderr; stanr never sets it.
python3 - "$SRC/runtime/src/island.cpp" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old_no_adjoint = """      // A refusal is not an error -- the replay still gives the right
      // gradient -- but it is worth being able to see, because it is the
      // difference between a region that is fast and one that merely
      // works, and nothing else about the model would show it.
      if (!gen && std::getenv("STANLI_DEBUG_ISLAND"))
        std::fprintf(stderr,
                     "island: no adjoint generated for a %zu-op region; "
                     "it will replay under var\\n",
                     j - i);
    }
"""
new_no_adjoint = """    }
"""
assert old_no_adjoint in text, "no-adjoint debug trace not found -- island.cpp changed upstream"
text = text.replace(old_no_adjoint, new_no_adjoint, 1)

old_cost = """      if (std::getenv("STANLI_DEBUG_ISLAND"))
        std::fprintf(stderr, "island? ops=%zu graph=%lld island=%lld\\n", j - i,
                     (long long)graph_cost, (long long)island_cost);
      if (graph_cost < island_cost) compiled = false;
"""
new_cost = """      if (graph_cost < island_cost) compiled = false;
"""
assert old_cost in text, "island-cost debug trace not found -- island.cpp changed upstream"
text = text.replace(old_cost, new_cost, 1)

old_carved = """        if (std::getenv("STANLI_DEBUG_ISLAND")) {
          const IslandProg& p = *static_cast<const IslandProg*>(is.udata);
          std::fprintf(stderr,
                       "island: ops=%zu instr=%zu regs=%d ins=%zu outs=%zu "
                       "adj=%zu\\n",
                       j - i, p.code.size(), p.n_regs, p.ins.size(),
                       p.out_regs.size(), p.adj.code.size());
          // Which instructions the region is made of, so a disagreement
          // with the replay can be attributed to an opcode rather than
          // guessed at.
          std::vector<int> hist(64, 0);
          for (const auto& I : p.code)
            if ((int)I.code < 64) ++hist[(size_t)I.code];
          std::fprintf(stderr, "island opcodes:");
          for (int c = 0; c < 64; ++c)
            if (hist[(size_t)c])
              std::fprintf(stderr, " %d:%d", c, hist[(size_t)c]);
          std::fprintf(stderr, "\\n");
        }
        ++carved;
"""
new_carved = """        ++carved;
"""
assert old_carved in text, "carved-island debug trace not found -- island.cpp changed upstream"
text = text.replace(old_carved, new_carved, 1)

old_include = "#include <cstdio>\n"
assert old_include in text, "cstdio include not found -- island.cpp changed upstream"
text = text.replace(old_include, "", 1)

open(path, "w").write(text)
EOF

# STANLI_DEBUG_ODE traces to stderr; stanr never sets it.
python3 - "$SRC/runtime/src/lower.cpp" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old_doc = """  // The op tail both ODE families share: report an interpreter fallback,
  // emit OP_ODE and hand the spec to the graph.
"""
new_doc = """  // The op tail both ODE families share: emit OP_ODE and hand the spec
  // to the graph.
"""
assert old_doc in text, "emit_ode doc comment not found -- lower.cpp changed upstream"
text = text.replace(old_doc, new_doc, 1)

old_trace = """    // Falling back to the interpreter is correct but ~30x slower, so make
    // it findable rather than silent.
    if (!spec->prog.ok && std::getenv("STANLI_DEBUG_ODE"))
      std::fprintf(stderr,
                   "stanli: ODE right-hand side %s falls back to the "
                   "interpreter: %s\\n",
                   spec->rhs_name.c_str(), spec->prog.why.c_str());
    Val v = emit_value(OP_ODE, {z0, theta}, N * S, result_si, {(int)N, (int)S});
"""
new_trace = """    Val v = emit_value(OP_ODE, {z0, theta}, N * S, result_si, {(int)N, (int)S});
"""
assert old_trace in text, "ODE fallback debug trace not found -- lower.cpp changed upstream"
text = text.replace(old_trace, new_trace, 1)

old_include = "#include <cstdio>\n"
assert old_include in text, "cstdio include not found -- lower.cpp changed upstream"
text = text.replace(old_include, "", 1)

open(path, "w").write(text)
EOF

# profile_report() is never called by stanr; rewrite it over
# <sstream>/<iomanip> instead of snprintf so nothing calls it either way.
python3 - "$SRC/runtime/src/executor.cpp" << 'EOF'
import sys

path = sys.argv[1]
text = open(path).read()

old_include = "#include <cstdio>\n"
new_include = "#include <iomanip>\n#include <sstream>\n"
assert old_include in text, "cstdio include not found -- executor.cpp changed upstream"
text = text.replace(old_include, new_include, 1)

old_report = """  char line[160];
  std::string out;
  std::snprintf(line, sizeof line, "%-22s %10s %12s %12s %6s %12s\\n", "opcode",
                "calls", "fwd ns", "bwd ns", "%", "elems");
  out += line;
  for (uint16_t op : order) {
    const ProfEntry& e = prof_[op];
    std::snprintf(line, sizeof line,
                  "%-22s %10lld %12lld %12lld %5.1f%% %12lld\\n",
                  opcode_name(op), (long long)e.calls, (long long)e.fwd_ns,
                  (long long)e.bwd_ns,
                  100.0 * (double)(e.fwd_ns + e.bwd_ns) / (double)grand,
                  (long long)e.elems);
    out += line;
  }
  std::snprintf(line, sizeof line, "%-22s %10s %12lld ns total\\n", "", "",
                (long long)grand);
  out += line;
  return out;
}
"""
new_report = """  std::ostringstream out;
  out << std::left << std::setw(22) << "opcode" << ' ' << std::right
      << std::setw(10) << "calls" << ' ' << std::setw(12) << "fwd ns" << ' '
      << std::setw(12) << "bwd ns" << ' ' << std::setw(6) << "%" << ' '
      << std::setw(12) << "elems" << '\\n';
  for (uint16_t op : order) {
    const ProfEntry& e = prof_[op];
    const double pct = 100.0 * (double)(e.fwd_ns + e.bwd_ns) / (double)grand;
    out << std::left << std::setw(22) << opcode_name(op) << ' ' << std::right
        << std::setw(10) << (long long)e.calls << ' ' << std::setw(12)
        << (long long)e.fwd_ns << ' ' << std::setw(12) << (long long)e.bwd_ns
        << ' ' << std::setw(5) << std::fixed << std::setprecision(1) << pct
        << '%' << ' ' << std::setw(12) << (long long)e.elems << '\\n';
  }
  out << std::left << std::setw(22) << "" << ' ' << std::right << std::setw(10)
      << "" << ' ' << std::setw(12) << (long long)grand << " ns total\\n";
  return out.str();
}
"""
assert old_report in text, "profile_report body not found -- executor.cpp changed upstream"
text = text.replace(old_report, new_report, 1)

open(path, "w").write(text)
EOF
