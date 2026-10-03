#include "ring_buffer.h"

#include <chrono>

using physiostream::RingBuffer;

// [[Rcpp::export]]
SEXP cpp_ring_create(int n_channels, int capacity, std::string dtype,
                     double memory_limit) {
  if (n_channels < 1 || capacity < 1) {
    Rcpp::stop("ring dimensions must be positive");
  }
  const std::size_t channels = static_cast<std::size_t>(n_channels);
  const std::size_t cap = static_cast<std::size_t>(capacity);
  if (channels > std::numeric_limits<std::size_t>::max() / cap) {
    Rcpp::stop("ring allocation product overflows size_t");
  }
  const long double bytes = static_cast<long double>(cap) *
    (static_cast<long double>(channels) * sizeof(std::atomic<double>) +
     sizeof(std::atomic<double>) + sizeof(std::atomic<std::uint64_t>));
  if (!std::isfinite(memory_limit) || memory_limit <= 0 ||
      bytes > static_cast<long double>(memory_limit)) {
    Rcpp::stop("ring allocation exceeds memory ceiling");
  }
  RingBuffer* raw = new RingBuffer(
    channels, cap, physiostream::parse_dtype(dtype)
  );
  Rcpp::XPtr<RingBuffer> ptr(raw, true);
  R_SetExternalPtrTag(ptr, physiostream::ring_pointer_tag());
  return ptr;
}

// [[Rcpp::export]]
Rcpp::List cpp_ring_push(SEXP pointer, Rcpp::NumericMatrix samples,
                         Rcpp::NumericVector timestamps) {
  RingBuffer& ring = physiostream::checked_ring(pointer);
  ring.push(samples, timestamps);
  return ring.stats();
}

// [[Rcpp::export]]
Rcpp::List cpp_ring_pull(SEXP pointer, int n) {
  if (n < 0) Rcpp::stop("pull count must be non-negative");
  RingBuffer& ring = physiostream::checked_ring(pointer);
  return ring.read(static_cast<std::uint64_t>(n), true, false);
}

// [[Rcpp::export]]
Rcpp::List cpp_ring_peek(SEXP pointer, int n, std::string from) {
  if (n < 0) Rcpp::stop("peek count must be non-negative");
  if (from != "oldest" && from != "latest") {
    Rcpp::stop("peek side must be exact");
  }
  RingBuffer& ring = physiostream::checked_ring(pointer);
  return ring.read(
    static_cast<std::uint64_t>(n), false, from == "latest"
  );
}

// [[Rcpp::export]]
Rcpp::List cpp_ring_reset(SEXP pointer) {
  RingBuffer& ring = physiostream::checked_ring(pointer);
  ring.reset();
  return ring.stats();
}

// [[Rcpp::export]]
Rcpp::List cpp_ring_stats(SEXP pointer) {
  RingBuffer& ring = physiostream::checked_ring(pointer);
  return ring.stats();
}

// [[Rcpp::export]]
void cpp_ring_finalize(SEXP pointer) {
  if (TYPEOF(pointer) != EXTPTRSXP) {
    Rcpp::stop("invalid ring-buffer pointer type");
  }
  if (R_ExternalPtrAddr(pointer) == nullptr) {
    return;
  }
  Rcpp::XPtr<RingBuffer> ptr(pointer);
  if (ptr) ptr->finalize();
  R_ClearExternalPtr(pointer);
}

// [[Rcpp::export]]
Rcpp::List cpp_ring_stress(int iterations, int capacity) {
  if (iterations < 1 || capacity < 1) {
    Rcpp::stop("stress dimensions must be positive");
  }
  const std::uint64_t total = static_cast<std::uint64_t>(iterations);
  const std::uint64_t cap = static_cast<std::uint64_t>(capacity);
  std::unique_ptr<std::uint64_t[]> slots(new std::uint64_t[capacity]);
  std::atomic<std::uint64_t> head(0);
  std::atomic<std::uint64_t> tail(0);
  std::atomic<bool> ordered(true);
  std::atomic<std::uint64_t> checksum(0);

  std::thread producer([&]() {
    for (std::uint64_t value = 0; value < total; ++value) {
      while (head.load(std::memory_order_relaxed) -
             tail.load(std::memory_order_acquire) >= cap) {
        std::this_thread::yield();
      }
      const std::uint64_t h = head.load(std::memory_order_relaxed);
      slots[h % cap] = value;
      head.store(h + 1, std::memory_order_release);
    }
  });

  std::thread consumer([&]() {
    std::uint64_t expected = 0;
    std::uint64_t sum = 0;
    while (expected < total) {
      const std::uint64_t t = tail.load(std::memory_order_relaxed);
      if (t == head.load(std::memory_order_acquire)) {
        std::this_thread::yield();
        continue;
      }
      const std::uint64_t value = slots[t % cap];
      if (value != expected) ordered.store(false, std::memory_order_relaxed);
      sum += value;
      ++expected;
      tail.store(t + 1, std::memory_order_release);
    }
    checksum.store(sum, std::memory_order_release);
  });

  producer.join();
  consumer.join();
  const long double expected_checksum =
    static_cast<long double>(total) *
    static_cast<long double>(total - 1) / 2.0L;
  return Rcpp::List::create(
    Rcpp::Named("ordered") = ordered.load(std::memory_order_acquire),
    Rcpp::Named("produced") = static_cast<double>(head.load()),
    Rcpp::Named("consumed") = static_cast<double>(tail.load()),
    Rcpp::Named("checksum") = static_cast<double>(checksum.load()),
    Rcpp::Named("expected_checksum") =
      static_cast<double>(expected_checksum),
    Rcpp::Named("dropped") = 0.0,
    Rcpp::Named("multiple_producers_supported") = false
  );
}

// [[Rcpp::export]]
double cpp_monotonic_ns() {
  using clock = std::chrono::steady_clock;
  static const clock::time_point origin = clock::now();
  const auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(
      clock::now() - origin);
  return static_cast<double>(elapsed.count());
}

// [[Rcpp::export]]
Rcpp::List cpp_pipeline_sos_rms(
    const Rcpp::NumericMatrix& samples,
    const Rcpp::NumericMatrix& sos,
    const Rcpp::NumericVector& zi_input,
    const Rcpp::NumericMatrix& buffer_input,
    const Rcpp::NumericVector& sums_input,
    int cursor,
    int filled) {
  const int n_samples = samples.nrow();
  const int n_channels = samples.ncol();
  const int n_sections = sos.nrow();
  const int window_samples = buffer_input.nrow();

  if (n_channels < 1 || n_sections < 1 || sos.ncol() != 6 ||
      window_samples < 1 || buffer_input.ncol() != n_channels ||
      sums_input.size() != n_channels || cursor < 0 ||
      cursor >= window_samples || filled < 0 || filled > window_samples) {
    Rcpp::stop("invalid governed SOS/RMS dimensions or state");
  }
  Rcpp::IntegerVector zi_dims = zi_input.attr("dim");
  if (zi_dims.size() != 3 || zi_dims[0] != n_sections ||
      zi_dims[1] != 2 || zi_dims[2] != n_channels) {
    Rcpp::stop("invalid governed SOS delay-state dimensions");
  }

  Rcpp::NumericVector zi = Rcpp::clone(zi_input);
  Rcpp::NumericMatrix buffer = Rcpp::clone(buffer_input);
  Rcpp::NumericVector sums = Rcpp::clone(sums_input);
  Rcpp::NumericMatrix rms(n_samples, n_channels);
  Rcpp::LogicalVector available(n_samples);

  for (int row = 0; row < n_samples; ++row) {
    const int next_filled = std::min(window_samples, filled + 1);
    available[row] = next_filled == window_samples;
    for (int channel = 0; channel < n_channels; ++channel) {
      double value = samples(row, channel);
      for (int section = 0; section < n_sections; ++section) {
        const int z1_index =
            section + n_sections * (0 + 2 * channel);
        const int z2_index =
            section + n_sections * (1 + 2 * channel);
        const double output = sos(section, 0) * value + zi[z1_index];
        const double next_z1 =
            sos(section, 1) * value - sos(section, 4) * output +
            zi[z2_index];
        const double next_z2 =
            sos(section, 2) * value - sos(section, 5) * output;
        zi[z1_index] = next_z1;
        zi[z2_index] = next_z2;
        value = output;
      }

      const double square = value * value;
      const double previous = buffer(cursor, channel);
      double sum = sums[channel] - previous + square;
      if (sum < 0 && sum > -1e-12 * std::max(1.0, sums[channel])) {
        sum = 0;
      }
      if (!std::isfinite(sum) || sum < 0) {
        Rcpp::stop("non-finite or negative governed RMS accumulation");
      }
      buffer(cursor, channel) = square;
      sums[channel] = sum;
      rms(row, channel) = std::sqrt(sum / static_cast<double>(next_filled));
    }
    cursor = (cursor + 1) % window_samples;
    filled = next_filled;
  }

  rms.attr("dimnames") = samples.attr("dimnames");
  zi.attr("dim") = zi_dims;
  return Rcpp::List::create(
      Rcpp::Named("rms") = rms,
      Rcpp::Named("available") = available,
      Rcpp::Named("zi") = zi,
      Rcpp::Named("rms_buffer") = buffer,
      Rcpp::Named("rms_sums") = sums,
      Rcpp::Named("rms_cursor") = cursor,
      Rcpp::Named("rms_filled") = filled);
}
