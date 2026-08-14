#include <cpp11.h>
#include <cpp11/declarations.h>
#include <cstdint>
#include <cstdio>

// FNV-1a 64-bit, used only to build cache-key fingerprints (no need for
// cryptographic strength). Each string's length is folded in before its
// bytes so ("ab","c") and ("a","bc") don't collide.
namespace {

constexpr std::uint64_t kOffsetBasis = 14695981039346656037ULL;
constexpr std::uint64_t kPrime = 1099511628211ULL;

std::uint64_t fnv1a(std::uint64_t hash, const unsigned char* data, std::size_t len) {
  for (std::size_t i = 0; i < len; ++i) {
    hash = (hash ^ data[i]) * kPrime;
  }
  return hash;
}

std::uint64_t mix_length(std::uint64_t hash, std::size_t len) {
  auto len64 = static_cast<std::uint64_t>(len);
  return fnv1a(hash, reinterpret_cast<const unsigned char*>(&len64), sizeof(len64));
}

}  // namespace

extern "C" SEXP stanr_hash_strings(SEXP strings_sexp) {
  BEGIN_CPP11
  cpp11::strings strings(strings_sexp);
  std::uint64_t hash = kOffsetBasis;
  for (const auto& element : strings) {
    std::string s(element);
    hash = mix_length(hash, s.size());
    hash = fnv1a(hash, reinterpret_cast<const unsigned char*>(s.data()), s.size());
  }

  char buf[17];
  std::snprintf(buf, sizeof(buf), "%016llx", static_cast<unsigned long long>(hash));
  return cpp11::as_sexp(std::string(buf));
  END_CPP11
}
