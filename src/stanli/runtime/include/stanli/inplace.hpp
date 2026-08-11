// Destructive functional updates. `v[n] = ...` under an unrolled loop
// lowers to a chain of OP_SET_INDEX ops, each of which copies the whole
// vector into a fresh slot: N writes cost O(N^2) time and arena. When a
// write is the last use of the vector it overwrites, it can mutate that
// vector in place instead, making the chain O(N).
#ifndef STANLI_INPLACE_HPP
#define STANLI_INPLACE_HPP

#include <stanli/graph.hpp>

#include <vector>

namespace stanli {

// Rewrites eligible OP_SET_INDEX ops to OP_SET_INDEX_INPLACE (whose out
// slot IS its first input) and renames later references to the dead
// output slot. `roots` are slots read from outside the op graph (jacobian
// terms, constrained-parameter views, the result): they are never
// overwritten and never renamed. Returns the number of writes rewritten.
// STANLI_NO_INPLACE=1 disables the pass.
// True for ops whose backward only routes adjoints and never reads the
// values of its inputs. The destructive rewrite below is sound only for
// vectors whose earlier readers all satisfy this: a backward pass runs
// after every write has landed, and kernels like log_sum_exp rebuild
// their var tape from the input buffer at that point. Exposed so
// tests/test_pass_safety.cpp can check every opcode claimed here really
// has the property, by poisoning inputs between the sweeps.
bool backward_ignores_input_values(uint16_t opcode);

int make_inplace_updates(Graph& g, const std::vector<int>& roots);

// Store-to-load forwarding, plus the dead ops it exposes. `mu[n] = e;` and
// a read of `mu[n]` in the same iteration lower to a write immediately
// followed by an OP_INDEX of the element just written; the read is
// replaced by the written value directly. When nothing then reads the
// vector, its writes are dead and go too, which is what leaves the plain
// per-lane arithmetic the re-roll pass can vectorize. Returns the number
// of ops removed. Disabled with the writes themselves under
// STANLI_NO_INPLACE.
int forward_stores_to_loads(Graph& g, const std::vector<int>& roots);

}  // namespace stanli

#endif
