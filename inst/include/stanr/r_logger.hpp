#ifndef STANR_R_LOGGER_HPP
#define STANR_R_LOGGER_HPP

#include <Rcpp.h>
#include <stan/callbacks/logger.hpp>
#include <mutex>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace stanr {

// Thread-safe buffered logger for Stan callbacks. Buffers messages behind a
// mutex so TBB workers can log without touching R streams; flush() on the
// main thread prints and retains everything in history_ for $output().
//
// Log levels: debug/info/warn -> Rcout, error/fatal -> Rcerr. Two gates:
// show_messages_ (progress) and show_exceptions_ (exception chatter);
// genuine errors are never suppressed.
class r_logger : public stan::callbacks::logger {
 public:
  enum class level { debug, info, warn, error, fatal };

 private:
  struct entry {
    level lv;
    bool exception;
    std::string msg;
  };

  std::vector<entry> buffer_;
  std::vector<std::string> history_;
  std::mutex mutex_;
  bool show_messages_;
  bool show_exceptions_;

  // Counters guarded by mutex_, classifying lines in known "exception
  // chatter" blocks (see push()). Best-effort in multi-chain runs where
  // concurrent workers interleave; $output() is always complete.
  int metropolis_pending_ = 0;  // open error-level blocks (may nest)
  int init_pending_ = 0;        // remaining continuation lines of a warn block

  static bool starts_with(const std::string& msg, const std::string& prefix) {
    return msg.compare(0, prefix.size(), prefix) == 0;
  }

  void push(level lv, std::string msg) {
    std::lock_guard<std::mutex> lock(mutex_);
    bool exception = false;
    if (lv == level::error) {
      if (starts_with(msg, "Informational Message:")) {
        exception = true;
        ++metropolis_pending_;
      } else if (metropolis_pending_ > 0) {
        exception = true;                        // e.what(), trailers, and ""
        if (msg.empty()) --metropolis_pending_;   // "" closes one block
      }
    } else if (lv == level::warn) {
      if (starts_with(msg, "Rejecting initial value")) {
        exception = true;
        init_pending_ = 2;                        // blocks are header + exactly 2
      } else if (init_pending_ > 0) {
        exception = true;
        --init_pending_;
      }
    }
    buffer_.push_back({lv, exception, std::move(msg)});
  }

 public:
  explicit r_logger(bool show_messages = true, bool show_exceptions = true)
      : show_messages_(show_messages), show_exceptions_(show_exceptions) {}

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
   * Flush buffered messages to the R console and history_. Main-thread only.
   * Exception-classified lines are gated by show_exceptions_; everything
   * else prints at error/fatal or when show_messages_. Clears buffer_.
   */
  void flush() {
    std::vector<entry> entries;
    {
      std::lock_guard<std::mutex> lock(mutex_);
      entries.swap(buffer_);
    }
    for (const auto& e : entries) {
      bool show = e.exception
          ? show_exceptions_
          : (e.lv >= level::error || show_messages_);
      switch (e.lv) {
        case level::debug:
        case level::info:
        case level::warn:
          if (show) Rprintf("%s\n", e.msg.c_str());
          break;
        case level::error:
        case level::fatal:
          if (show) REprintf("%s\n", e.msg.c_str());
          break;
      }
      history_.push_back(e.msg);
    }
  }

  /// Main-thread-only, like flush(): messages from all flush() calls.
  const std::vector<std::string>& history() const { return history_; }
};

}  // namespace stanr

#endif  // STANR_R_LOGGER_HPP
