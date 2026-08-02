#ifndef NEWSTAN_R_LOGGER_HPP
#define NEWSTAN_R_LOGGER_HPP

#include <Rcpp.h>
#include <stan/callbacks/logger.hpp>
#include <mutex>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace newstan {

/**
 * Thread-safe buffered logger for Stan callbacks.
 *
 * Buffers log messages in a mutex-protected vector so that TBB worker
 * threads can log safely without accessing R streams.  Messages are
 * flushed to the R console on the main thread via flush(), and every
 * flushed message is retained in history_ for $output() regardless of
 * verbose_ -- a quiet run still needs to be able to report its log
 * messages after the fact, matching cmdstanr's output-file behavior.
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
  std::vector<std::string> history_;
  std::mutex mutex_;
  bool verbose_;

  void push(level lv, std::string msg) {
    std::lock_guard<std::mutex> lock(mutex_);
    buffer_.push_back({lv, std::move(msg)});
  }

 public:
  explicit r_logger(bool verbose = true) : verbose_(verbose) {}

  void debug(const std::string& message) override { push(level::debug, message); }

  void debug(const std::stringstream& message) override {
    push(level::debug, message.str());
  }

  void info(const std::string& message) override { push(level::info, message); }

  void info(const std::stringstream& message) override {
    push(level::info, message.str());
  }

  void warn(const std::string& message) override { push(level::warn, message); }

  void warn(const std::stringstream& message) override {
    push(level::warn, message.str());
  }

  void error(const std::string& message) override { push(level::error, message); }

  void error(const std::stringstream& message) override {
    push(level::error, message.str());
  }

  void fatal(const std::string& message) override { push(level::fatal, message); }

  void fatal(const std::stringstream& message) override {
    push(level::fatal, message.str());
  }

  /**
   * Flush all buffered messages to the R console and into history_.
   *
   * MUST be called from the main R thread.  When verbose_ is true,
   * prints debug/info/warn to stdout; error/fatal always print to
   * stderr.  Every flushed message is appended to history_ regardless
   * of verbose_, so $output() can recover it later.  Clears buffer_;
   * history_ accumulates across calls and is never cleared here.
   */
  void flush() {
    std::vector<entry> entries;
    {
      std::lock_guard<std::mutex> lock(mutex_);
      entries.swap(buffer_);
    }
    for (const auto& e : entries) {
      switch (e.lv) {
        case level::debug:
        case level::info:
        case level::warn:
          if (verbose_) Rprintf("%s\n", e.msg.c_str());
          break;
        case level::error:
        case level::fatal:
          REprintf("%s\n", e.msg.c_str());
          break;
      }
      history_.push_back(e.msg);
    }
  }

  /// Main-thread-only, like flush(): accumulated messages from all flush() calls.
  const std::vector<std::string>& history() const { return history_; }
};

}  // namespace newstan

#endif  // NEWSTAN_R_LOGGER_HPP
