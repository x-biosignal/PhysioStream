#ifndef PHYSIOSTREAM_RING_BUFFER_H
#define PHYSIOSTREAM_RING_BUFFER_H

#include <Rcpp.h>

#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace physiostream {

constexpr std::uint64_t kMagic = UINT64_C(0x505354524D425546);
constexpr std::uint64_t kMaxExactRInteger = UINT64_C(9007199254740991);

inline SEXP ring_pointer_tag() {
  return Rf_install("PhysioStream::RingBuffer/v1");
}

enum class DType {
  Float64,
  Float32,
  Int32,
  Int16,
  Int8
};

inline DType parse_dtype(const std::string& dtype) {
  if (dtype == "float64") return DType::Float64;
  if (dtype == "float32") return DType::Float32;
  if (dtype == "int32") return DType::Int32;
  if (dtype == "int16") return DType::Int16;
  if (dtype == "int8") return DType::Int8;
  Rcpp::stop("unsupported numeric ring dtype `%s`", dtype);
}

inline double dtype_min(DType dtype) {
  switch (dtype) {
    case DType::Int32:
      return static_cast<double>(std::numeric_limits<std::int32_t>::min());
    case DType::Int16:
      return static_cast<double>(std::numeric_limits<std::int16_t>::min());
    case DType::Int8:
      return static_cast<double>(std::numeric_limits<std::int8_t>::min());
    default:
      return -std::numeric_limits<double>::infinity();
  }
}

inline double dtype_max(DType dtype) {
  switch (dtype) {
    case DType::Int32:
      return static_cast<double>(std::numeric_limits<std::int32_t>::max());
    case DType::Int16:
      return static_cast<double>(std::numeric_limits<std::int16_t>::max());
    case DType::Int8:
      return static_cast<double>(std::numeric_limits<std::int8_t>::max());
    default:
      return std::numeric_limits<double>::infinity();
  }
}

inline double store_value(double value, DType dtype) {
  if (dtype == DType::Float32) {
    return static_cast<double>(static_cast<float>(value));
  }
  return value;
}

class RingBuffer {
 public:
  RingBuffer(std::size_t n_channels, std::size_t capacity, DType dtype)
      : magic_(kMagic),
        finalized_(false),
        n_channels_(n_channels),
        capacity_(capacity),
        dtype_(dtype),
        samples_(new std::atomic<double>[capacity * n_channels]),
        timestamps_(new std::atomic<double>[capacity]),
        sequences_(new std::atomic<std::uint64_t>[capacity]),
        write_sequence_(0),
        read_sequence_(0),
        total_pushed_(0),
        total_pulled_(0),
        total_dropped_(0),
        reset_count_(0),
        has_last_timestamp_(false),
        last_timestamp_(0.0) {
    for (std::size_t i = 0; i < capacity_ * n_channels_; ++i) {
      samples_[i].store(0.0, std::memory_order_relaxed);
    }
    for (std::size_t i = 0; i < capacity_; ++i) {
      timestamps_[i].store(0.0, std::memory_order_relaxed);
      sequences_[i].store(
        std::numeric_limits<std::uint64_t>::max(),
        std::memory_order_relaxed
      );
    }
    if (!samples_[0].is_lock_free() ||
        !timestamps_[0].is_lock_free() ||
        !sequences_[0].is_lock_free()) {
      Rcpp::stop("platform does not provide lock-free ring atomics");
    }
  }

  RingBuffer(const RingBuffer&) = delete;
  RingBuffer& operator=(const RingBuffer&) = delete;

  bool valid() const {
    return magic_ == kMagic && !finalized_.load(std::memory_order_acquire);
  }

  void finalize() {
    finalized_.store(true, std::memory_order_release);
    magic_ = 0;
  }

  std::size_t n_channels() const { return n_channels_; }
  std::size_t capacity() const { return capacity_; }
  DType dtype() const { return dtype_; }

  std::uint64_t write_sequence() const {
    return write_sequence_.load(std::memory_order_acquire);
  }

  std::uint64_t read_sequence() const {
    return read_sequence_.load(std::memory_order_acquire);
  }

  std::uint64_t fill() const {
    const std::uint64_t w = write_sequence();
    const std::uint64_t r = read_sequence();
    return w >= r ? w - r : 0;
  }

  void validate_push(const Rcpp::NumericMatrix& values,
                     const Rcpp::NumericVector& times) const {
    if (values.nrow() < 1 || static_cast<std::size_t>(values.ncol()) != n_channels_) {
      Rcpp::stop("sample matrix has invalid dimensions");
    }
    if (times.size() != values.nrow()) {
      Rcpp::stop("timestamps do not match sample rows");
    }
    const std::uint64_t n = static_cast<std::uint64_t>(values.nrow());
    const std::uint64_t write = write_sequence();
    if (write > kMaxExactRInteger ||
        n - 1 > kMaxExactRInteger - write) {
      Rcpp::stop("sequence identity would exceed exact R double precision");
    }
    if (total_pushed_.load(std::memory_order_relaxed) >
        kMaxExactRInteger - n) {
      Rcpp::stop("push counter would exceed exact R double precision");
    }
    double previous = has_last_timestamp_ ? last_timestamp_ :
      -std::numeric_limits<double>::infinity();
    for (R_xlen_t i = 0; i < times.size(); ++i) {
      const double timestamp = times[i];
      if (!std::isfinite(timestamp) || timestamp <= previous) {
        Rcpp::stop("timestamps must be finite and strictly increasing");
      }
      previous = timestamp;
      for (std::size_t j = 0; j < n_channels_; ++j) {
        const double value = values(i, static_cast<R_xlen_t>(j));
        if (!std::isfinite(value)) {
          Rcpp::stop("samples must be finite");
        }
        if (dtype_ == DType::Float32 &&
            !std::isfinite(static_cast<float>(value))) {
          Rcpp::stop("sample overflows finite float32 storage");
        }
        if (dtype_ == DType::Int32 || dtype_ == DType::Int16 ||
            dtype_ == DType::Int8) {
          if (value != std::trunc(value) ||
              value < dtype_min(dtype_) || value > dtype_max(dtype_)) {
            Rcpp::stop("sample is not exactly representable by integer dtype");
          }
        }
      }
    }
  }

  void push(const Rcpp::NumericMatrix& values,
            const Rcpp::NumericVector& times) {
    validate_push(values, times);
    const std::uint64_t n = static_cast<std::uint64_t>(values.nrow());
    for (std::uint64_t i = 0; i < n; ++i) {
      const std::uint64_t write =
        write_sequence_.load(std::memory_order_relaxed);
      std::uint64_t read = read_sequence_.load(std::memory_order_acquire);
      if (write - read >= capacity_) {
        while (write - read >= capacity_) {
          if (read_sequence_.compare_exchange_weak(
                read, read + 1,
                std::memory_order_acq_rel,
                std::memory_order_acquire)) {
            total_dropped_.fetch_add(1, std::memory_order_relaxed);
            break;
          }
        }
      }
      const std::size_t slot = static_cast<std::size_t>(write % capacity_);
      sequences_[slot].store(
        std::numeric_limits<std::uint64_t>::max(),
        std::memory_order_release
      );
      for (std::size_t j = 0; j < n_channels_; ++j) {
        samples_[slot * n_channels_ + j].store(
          store_value(values(static_cast<R_xlen_t>(i),
                             static_cast<R_xlen_t>(j)), dtype_),
          std::memory_order_relaxed
        );
      }
      timestamps_[slot].store(
        times[static_cast<R_xlen_t>(i)],
        std::memory_order_relaxed
      );
      sequences_[slot].store(write, std::memory_order_release);
      write_sequence_.store(write + 1, std::memory_order_release);
    }
    total_pushed_.fetch_add(n, std::memory_order_relaxed);
    last_timestamp_ = times[times.size() - 1];
    has_last_timestamp_ = true;
  }

  Rcpp::List read(std::uint64_t n, bool consume, bool latest) {
    for (int attempt = 0; attempt < 1000; ++attempt) {
      const std::uint64_t write = write_sequence();
      std::uint64_t read = read_sequence();
      const std::uint64_t available = write - read;
      const std::uint64_t count = std::min(n, available);
      const std::uint64_t begin = latest ? write - count : read;

      Rcpp::NumericMatrix values(
        static_cast<R_xlen_t>(count), static_cast<R_xlen_t>(n_channels_));
      Rcpp::NumericVector times(static_cast<R_xlen_t>(count));
      Rcpp::NumericVector sequence(static_cast<R_xlen_t>(count));
      bool stable = true;
      for (std::uint64_t i = 0; i < count && stable; ++i) {
        const std::uint64_t seq = begin + i;
        const std::size_t slot = static_cast<std::size_t>(seq % capacity_);
        if (sequences_[slot].load(std::memory_order_acquire) != seq) {
          stable = false;
          break;
        }
        for (std::size_t j = 0; j < n_channels_; ++j) {
          values(static_cast<R_xlen_t>(i), static_cast<R_xlen_t>(j)) =
            samples_[slot * n_channels_ + j].load(std::memory_order_relaxed);
        }
        times[static_cast<R_xlen_t>(i)] =
          timestamps_[slot].load(std::memory_order_relaxed);
        if (sequences_[slot].load(std::memory_order_acquire) != seq) {
          stable = false;
          break;
        }
        sequence[static_cast<R_xlen_t>(i)] = static_cast<double>(seq);
      }
      if (!stable) {
        std::this_thread::yield();
        continue;
      }
      if (consume && count) {
        if (total_pulled_.load(std::memory_order_relaxed) >
            kMaxExactRInteger - count) {
          Rcpp::stop("pull counter would exceed exact R double precision");
        }
        if (!read_sequence_.compare_exchange_strong(
              read, read + count,
              std::memory_order_acq_rel,
              std::memory_order_acquire)) {
          std::this_thread::yield();
          continue;
        }
        total_pulled_.fetch_add(count, std::memory_order_relaxed);
      }
      return Rcpp::List::create(
        Rcpp::Named("samples") = values,
        Rcpp::Named("timestamps") = times,
        Rcpp::Named("sequence") = sequence,
        Rcpp::Named("count") = static_cast<int>(count),
        Rcpp::Named("stats_after") = stats()
      );
    }
    Rcpp::stop("ring read could not obtain a stable SPSC snapshot");
  }

  void reset() {
    read_sequence_.store(write_sequence(), std::memory_order_release);
    has_last_timestamp_ = false;
    last_timestamp_ = 0.0;
    if (reset_count_.load(std::memory_order_relaxed) == kMaxExactRInteger) {
      Rcpp::stop("reset counter would exceed exact R double precision");
    }
    reset_count_.fetch_add(1, std::memory_order_relaxed);
  }

  Rcpp::List stats() const {
    const std::uint64_t write = write_sequence();
    const std::uint64_t read = read_sequence();
    const std::uint64_t available = write - read;
    Rcpp::RObject oldest_sequence = R_NilValue;
    Rcpp::RObject newest_sequence = R_NilValue;
    Rcpp::RObject oldest_timestamp = R_NilValue;
    Rcpp::RObject newest_timestamp = R_NilValue;
    if (available) {
      const std::size_t oldest_slot =
        static_cast<std::size_t>(read % capacity_);
      const std::size_t newest_slot =
        static_cast<std::size_t>((write - 1) % capacity_);
      oldest_sequence = Rcpp::wrap(static_cast<double>(
        sequences_[oldest_slot].load(std::memory_order_acquire)
      ));
      newest_sequence = Rcpp::wrap(static_cast<double>(
        sequences_[newest_slot].load(std::memory_order_acquire)
      ));
      oldest_timestamp = Rcpp::wrap(
        timestamps_[oldest_slot].load(std::memory_order_relaxed)
      );
      newest_timestamp = Rcpp::wrap(
        timestamps_[newest_slot].load(std::memory_order_relaxed)
      );
    }
    return Rcpp::List::create(
      Rcpp::Named("capacity") = static_cast<int>(capacity_),
      Rcpp::Named("fill") = static_cast<int>(available),
      Rcpp::Named("n_channels") = static_cast<int>(n_channels_),
      Rcpp::Named("dtype") = dtype_string(),
      Rcpp::Named("storage_precision_bits") =
        dtype_ == DType::Float32 ? 24 : 53,
      Rcpp::Named("lock_free") = true,
      Rcpp::Named("total_pushed") =
        static_cast<double>(total_pushed_.load(std::memory_order_relaxed)),
      Rcpp::Named("total_pulled") =
        static_cast<double>(total_pulled_.load(std::memory_order_relaxed)),
      Rcpp::Named("total_dropped") =
        static_cast<double>(total_dropped_.load(std::memory_order_relaxed)),
      Rcpp::Named("reset_count") =
        static_cast<double>(reset_count_.load(std::memory_order_relaxed)),
      Rcpp::Named("oldest_sequence") = oldest_sequence,
      Rcpp::Named("newest_sequence") = newest_sequence,
      Rcpp::Named("oldest_timestamp") = oldest_timestamp,
      Rcpp::Named("newest_timestamp") = newest_timestamp,
      Rcpp::Named("loss_observed") =
        total_dropped_.load(std::memory_order_relaxed) > 0
    );
  }

 private:
  std::string dtype_string() const {
    switch (dtype_) {
      case DType::Float64: return "float64";
      case DType::Float32: return "float32";
      case DType::Int32: return "int32";
      case DType::Int16: return "int16";
      case DType::Int8: return "int8";
    }
    return "unknown";
  }

  std::uint64_t magic_;
  std::atomic<bool> finalized_;
  std::size_t n_channels_;
  std::size_t capacity_;
  DType dtype_;
  std::unique_ptr<std::atomic<double>[]> samples_;
  std::unique_ptr<std::atomic<double>[]> timestamps_;
  std::unique_ptr<std::atomic<std::uint64_t>[]> sequences_;
  std::atomic<std::uint64_t> write_sequence_;
  std::atomic<std::uint64_t> read_sequence_;
  std::atomic<std::uint64_t> total_pushed_;
  std::atomic<std::uint64_t> total_pulled_;
  std::atomic<std::uint64_t> total_dropped_;
  std::atomic<std::uint64_t> reset_count_;
  bool has_last_timestamp_;
  double last_timestamp_;
};

inline void ring_finalizer(RingBuffer* ptr) {
  if (ptr != nullptr) {
    ptr->finalize();
  }
}

inline RingBuffer& checked_ring(SEXP pointer) {
  if (TYPEOF(pointer) != EXTPTRSXP) {
    Rcpp::stop("invalid ring-buffer pointer type");
  }
  if (R_ExternalPtrAddr(pointer) == nullptr) {
    Rcpp::stop("ring-buffer pointer is null or was restored from serialization");
  }
  if (R_ExternalPtrTag(pointer) != ring_pointer_tag()) {
    Rcpp::stop("ring-buffer pointer has an invalid schema tag");
  }
  Rcpp::XPtr<RingBuffer> ptr(pointer);
  if (!ptr || !ptr->valid()) {
    Rcpp::stop("ring-buffer pointer is finalized or has invalid schema magic");
  }
  return *ptr;
}

}  // namespace physiostream

#endif
