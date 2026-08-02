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
 * show_messages_/show_exceptions_ -- a quiet run still needs to be able to
 * report its log messages after the fact, matching cmdstanr's output-file
 * behavior.
 *
 * Log-level mapping (matches stream_logger's typical usage):
 *   debug, info, warn  -> Rcpp::Rcout
 *   error, fatal       -> Rcpp::Rcerr
 *
 * Two independent print gates: show_messages_ controls progress/
 * informational output; show_exceptions_ controls "exception chatter"
 * (Metropolis proposal rejections, initial-value rejections) regardless of
 * its (often error/warn) log level. Genuine errors are never suppressed.
 */
class r_logger : public stan::callbacks::logger {
 public:
  /// Internal log level
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

  // Counters guarded by mutex_, used to classify lines belonging to known
  // "exception chatter" blocks (see push() below).
  //
  // Known limitation: in multi-chain runs, Metropolis rejection blocks from
  // concurrent worker threads can interleave in the shared logger, and a
  // bare e.what() line between another block's header and terminator shares
  // the same counter -- so classification of bare lines is best-effort.
  // Worst case a rejection line prints (or an empty line is swallowed)
  // despite the flag; $output() is always complete regardless. cmdstanr
  // documents the same imperfection for its implementation ("will not
  // necessarily silence all messages").
  int metropolis_pending_ = 0;  // open error-level blocks (multi-chain: may nest)
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
   * Flush all buffered messages to the R console and into history_.
   *
   * MUST be called from the main R thread.  Print decision per entry:
   * exception-classified lines are gated by show_exceptions_; everything
   * else prints when at error/fatal level (never suppressed) or when
   * show_messages_ is true.  Stream routing is unchanged: debug/info/warn
   * -> stdout, error/fatal -> stderr.  Every flushed message is appended
   * to history_ regardless of what printed, so $output() can recover it
   * later.  Clears buffer_; history_ accumulates across calls and is never
   * cleared here.
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

  /// Main-thread-only, like flush(): accumulated messages from all flush() calls.
  const std::vector<std::string>& history() const { return history_; }
};

}  // namespace newstan

#endif  // NEWSTAN_R_LOGGER_HPP
