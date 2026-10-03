lsl_test_xml <- function(name = "test-stream", type = "EEG",
                         n_channels = 2L, nominal_srate = 100,
                         format = "double64", source_id = "test-source",
                         labels = paste0("ch", seq_len(n_channels)),
                         units = rep("uV", n_channels),
                         uid = "test-uid", created_at = 123.5) {
  channel_xml <- vapply(seq_len(n_channels), function(i) {
    label <- if (is.null(labels)) "" else labels[[i]]
    unit <- if (is.null(units)) "" else units[[i]]
    paste0(
      "<channel>",
      if (nzchar(label)) paste0("<label>", label, "</label>") else "",
      if (nzchar(unit)) paste0("<unit>", unit, "</unit>") else "",
      "</channel>"
    )
  }, character(1))
  paste0(
    "<?xml version=\"1.0\"?><info>",
    "<name>", name, "</name>",
    "<type>", type, "</type>",
    "<channel_count>", n_channels, "</channel_count>",
    "<channel_format>", format, "</channel_format>",
    "<source_id>", source_id, "</source_id>",
    "<nominal_srate>", base::format(nominal_srate, scientific = FALSE),
    "</nominal_srate>",
    "<version>1.10</version><created_at>", created_at, "</created_at>",
    "<uid>", uid, "</uid><session_id>default</session_id>",
    "<hostname>localhost</hostname><desc><channels>",
    paste0(channel_xml, collapse = ""),
    "</channels><manufacturer>PhysioStream test</manufacturer></desc></info>"
  )
}

lsl_test_info <- function(...) {
  PhysioStream:::.lsl_record_to_stream_info(
    PhysioStream:::.lsl_descriptor_record(
      lsl_test_xml(...), library_version = "test-liblsl"
    )
  )
}

lsl_test_open_inlet <- function(info, chunks = list(), capacity = 8L,
                                max_chunk = 8L, marker_capacity = 8L) {
  x <- lslInlet(
    info,
    capacity = capacity,
    max_chunk = max_chunk,
    marker_capacity = marker_capacity
  )
  handle <- new.env(parent = emptyenv())
  handle$chunks <- chunks
  adapter <- new.env(parent = emptyenv())
  adapter$pull_chunk <- function(handle, timeout, max_samples, dtype) {
    if (!length(handle$chunks)) {
      return(list(list(), numeric()))
    }
    chunk <- handle$chunks[[1L]]
    handle$chunks <- handle$chunks[-1L]
    chunk
  }
  x@runtime$adapter <- adapter
  x@runtime$handle <- handle
  x@runtime$state <- "open"
  x@state <- "open"
  x
}

lsl_test_open_outlet <- function(info, fail_at = NULL) {
  x <- lslOutlet(info)
  handle <- new.env(parent = emptyenv())
  handle$calls <- list()
  adapter <- new.env(parent = emptyenv())
  adapter$push_chunk <- function(handle, samples, pushthrough) {
    handle$calls[[length(handle$calls) + 1L]] <- list(
      samples = samples, timestamp = NULL, pushthrough = pushthrough
    )
  }
  adapter$push_sample <- function(handle, sample, timestamp, pushthrough) {
    call_index <- length(handle$calls) + 1L
    if (!is.null(fail_at) && call_index == fail_at) {
      stop("injected transport failure")
    }
    handle$calls[[call_index]] <- list(
      sample = sample, timestamp = timestamp, pushthrough = pushthrough
    )
  }
  x@runtime$adapter <- adapter
  x@runtime$handle <- handle
  x@runtime$state <- "open"
  x@state <- "open"
  x
}

lsl_close_quietly <- function(x) {
  try(streamClose(x), silent = TRUE)
  invisible(NULL)
}
