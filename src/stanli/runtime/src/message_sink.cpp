#include <stanli/message_sink.hpp>

#include <mutex>
#include <utility>

namespace stanli {

namespace {

std::mutex& sink_mutex() {
  static std::mutex m;
  return m;
}

MessageSink& current_sink() {
  static MessageSink s;
  return s;
}

}  // namespace

void set_message_sink(MessageSink s) {
  std::lock_guard<std::mutex> lock(sink_mutex());
  current_sink() = std::move(s);
}

void emit_message(const std::string& text) {
  std::lock_guard<std::mutex> lock(sink_mutex());
  if (current_sink()) current_sink()(text.data(), text.size());
  // No sink installed: stanr does not call set_message_sink() (yet), and
  // the default fallback wrote to the process's raw stdout, which R CMD
  // check flags. Drop the message rather than write around R's console.
}

}  // namespace stanli
