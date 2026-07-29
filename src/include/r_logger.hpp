#ifndef NEWSTAN_R_LOGGER_HPP
#define NEWSTAN_R_LOGGER_HPP

#include <Rcpp.h>
#include <stan/callbacks/logger.hpp>
#include <mutex>
#include <string>
#include <vector>

namespace newstan {

/**
 * Thread-safe buffered logger for Stan callbacks.
 *
 * Buffers log messages in a mutex-protected vector so that TBB worker
 * threads can log safely without accessing R streams.  Messages are
 * flushed to the R console on the main thread via flush().
 *
 * Log-level mapping (matches stream_logger's typical usage):
 *   debug, info, warn  -> Rcpp::Rcout
 *   error, fatal       -> Rcpp::Rcerr
 */
class r_logger : public stan::callbacks::logger {
 public:
  /// Internal log level
  enum class level { debug, info, warn, error, fatal };

 private:
  struct entry {
    level lv;
    std::string msg;
  };

  std::vector<entry> buffer_;
  std::mutex mutex_;

  void push(level lv, const std::string& msg) {
    std::lock_guard<std::mutex> lock(mutex_);
    buffer_.push_back({lv, msg});
  }

 public:
  // ── debug ──────────────────────────────────────────────────────
  void debug(const std::string& message) override { push(level::debug, message); }

  void debug(const std::stringstream& message) override {
    push(level::debug, message.str());
  }

  // ── info ───────────────────────────────────────────────────────
  void info(const std::string& message) override { push(level::info, message); }

  void info(const std::stringstream& message) override {
    push(level::info, message.str());
  }

  // ── warn ───────────────────────────────────────────────────────
  void warn(const std::string& message) override { push(level::warn, message); }

  void warn(const std::stringstream& message) override {
    push(level::warn, message.str());
  }

  // ── error ──────────────────────────────────────────────────────
  void error(const std::string& message) override { push(level::error, message); }

  void error(const std::stringstream& message) override {
    push(level::error, message.str());
  }

  // ── fatal ──────────────────────────────────────────────────────
  void fatal(const std::string& message) override { push(level::fatal, message); }

  void fatal(const std::stringstream& message) override {
    push(level::fatal, message.str());
  }

  /**
   * Flush all buffered messages to the R console.
   *
   * MUST be called from the main R thread.  Prints debug/info/warn
   * to stdout and error/fatal to stderr, then clears the buffer.
   */
  void flush() {
    std::lock_guard<std::mutex> lock(mutex_);
    for (const auto& e : buffer_) {
      switch (e.lv) {
        case level::debug:
        case level::info:
        case level::warn:
          Rcpp::Rcout << e.msg << std::endl;
          break;
        case level::error:
        case level::fatal:
          Rcpp::Rcerr << e.msg << std::endl;
          break;
      }
    }
    buffer_.clear();
  }
};

}  // namespace newstan

#endif  // NEWSTAN_R_LOGGER_HPP
