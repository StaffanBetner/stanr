#ifndef STANR_R_VECTOR_COPY_HPP
#define STANR_R_VECTOR_COPY_HPP

#include <cpp11.hpp>

#include <cstddef>
#include <stdexcept>
#include <vector>

namespace stanr {

namespace internal {

template <typename T>
static inline std::size_t checked_append_size(R_xlen_t input_size,
                                              const std::vector<T>& output) {
  if (input_size < 0) {
    throw std::length_error("Cannot copy an R vector with a negative length.");
  }
  const std::size_t size = static_cast<std::size_t>(input_size);
  if (static_cast<R_xlen_t>(size) != input_size
      || size > output.max_size() - output.size()) {
    throw std::length_error("R vector is too long to copy.");
  }
  return size;
}

// Copy through R's region API instead of cpp11's buffered iterators. The
// latter contain a 4096-element array, and range-based vector construction can
// make enough iterator copies to produce very large stack frames on GCC. On
// affected AArch64 GCC versions, stack probing for those frames also produces
// invalid unwind information, preventing cpp11 exceptions from being caught
// (GCC PR c++/119610).
static inline void append_real_values(const cpp11::doubles& input,
                                      std::vector<double>& output) {
  const R_xlen_t input_size = input.size();
  const std::size_t size = checked_append_size(input_size, output);
  const std::size_t output_offset = output.size();
  output.resize(output_offset + size);

  R_xlen_t offset = 0;
  while (offset < input_size) {
    const R_xlen_t requested = input_size - offset;
    const R_xlen_t copied = cpp11::safe[REAL_GET_REGION](
        input.data(), offset, requested,
        output.data() + output_offset + static_cast<std::size_t>(offset));
    if (copied < 0 || copied > requested) {
      throw std::runtime_error("REAL_GET_REGION returned an invalid length.");
    }
    if (copied == 0) {
      output[output_offset + static_cast<std::size_t>(offset)] =
          cpp11::safe[REAL_ELT](input.data(), offset);
      ++offset;
    } else {
      offset += copied;
    }
  }
}

static inline std::vector<double> copy_real_values(
    const cpp11::doubles& input) {
  std::vector<double> output;
  append_real_values(input, output);
  return output;
}

static inline void append_integer_values(const cpp11::integers& input,
                                         std::vector<int>& output) {
  const R_xlen_t input_size = input.size();
  const std::size_t size = checked_append_size(input_size, output);
  const std::size_t output_offset = output.size();
  output.resize(output_offset + size);

  R_xlen_t offset = 0;
  while (offset < input_size) {
    const R_xlen_t requested = input_size - offset;
    const R_xlen_t copied = cpp11::safe[INTEGER_GET_REGION](
        input.data(), offset, requested,
        output.data() + output_offset + static_cast<std::size_t>(offset));
    if (copied < 0 || copied > requested) {
      throw std::runtime_error("INTEGER_GET_REGION returned an invalid length.");
    }
    if (copied == 0) {
      output[output_offset + static_cast<std::size_t>(offset)] =
          cpp11::safe[INTEGER_ELT](input.data(), offset);
      ++offset;
    } else {
      offset += copied;
    }
  }
}

static inline std::vector<int> copy_integer_values(
    const cpp11::integers& input) {
  std::vector<int> output;
  append_integer_values(input, output);
  return output;
}

}  // namespace internal

}  // namespace stanr

#endif  // STANR_R_VECTOR_COPY_HPP
