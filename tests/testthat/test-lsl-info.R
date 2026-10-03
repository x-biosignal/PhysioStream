test_that("all governed LSL dtypes map exactly", {
  formats <- c(
    float32 = "float32",
    double64 = "float64",
    int32 = "int32",
    int16 = "int16",
    int8 = "int8",
    string = "string"
  )
  for (format_name in names(formats)) {
    rate <- if (format_name == "string") 0 else 100
    type <- if (format_name == "string") "Markers" else "EEG"
    observed <- lsl_test_info(
      type = type,
      nominal_srate = rate,
      format = format_name
    )
    expect_identical(streamDtype(observed), unname(formats[[format_name]]))
    expect_equal(streamRate(observed), rate, tolerance = 0)
    expect_identical(streamChannels(observed), c("ch1", "ch2"))
    expect_identical(observed@channel_units, c("uV", "uV"))
    expect_match(
      observed@metadata$lsl$descriptor_sha256, "^[0-9a-f]{64}$"
    )
    expect_identical(observed@metadata$lsl$mapping_schema, "1.0.0")
  }
})

test_that("unsupported rate and dtype combinations fail loudly", {
  expect_error(
    lsl_test_info(format = "int64"),
    "unsupported LSL channel format"
  )
  expect_error(
    lsl_test_info(format = "string", nominal_srate = 100),
    "string LSL streams"
  )
  expect_error(
    lsl_test_info(format = "double64", nominal_srate = 0),
    "numeric LSL streams"
  )
})

test_that("channel label and unit cardinality is governed", {
  no_labels <- lsl_test_info(labels = NULL, units = NULL)
  expect_identical(
    streamChannels(no_labels), c("channel_001", "channel_002")
  )
  expect_null(no_labels@channel_units)

  expect_error(
    lsl_test_info(labels = c("C3", ""), units = NULL),
    "all present or all absent"
  )
  expect_error(
    lsl_test_info(labels = c("C3", "C3"), units = NULL),
    "unique"
  )
  expect_error(
    lsl_test_info(labels = c("C3", "C4"), units = c("uV", "")),
    "all present or all absent"
  )
  wrong_count <- sub(
    "<channel_count>2</channel_count>",
    "<channel_count>3</channel_count>",
    lsl_test_xml(), fixed = TRUE
  )
  expect_error(
    PhysioStream:::.lsl_descriptor_record(wrong_count),
    "count differs"
  )
})

test_that("descriptor hashing is deterministic and identity-sensitive", {
  xml <- lsl_test_xml()
  first <- PhysioStream:::.lsl_descriptor_record(xml)
  second <- PhysioStream:::.lsl_descriptor_record(xml)
  expect_identical(
    first$lsl$descriptor_sha256, second$lsl$descriptor_sha256
  )
  changed <- PhysioStream:::.lsl_descriptor_record(
    sub("<uid>test-uid</uid>", "<uid>changed</uid>", xml, fixed = TRUE)
  )
  expect_false(identical(
    first$lsl$descriptor_sha256, changed$lsl$descriptor_sha256
  ))
  expect_error(
    PhysioStream:::.lsl_descriptor_record("<not-info/>"),
    "root"
  )
})

test_that("outlet metadata rejects silent schema and channel drift", {
  make_info <- function(lsl) {
    streamInfo(
      "out", "EEG", c("C3", "C4"), 100, "float64", "source", "lsl",
      c("uV", "uV"), metadata = list(lsl = lsl)
    )
  }
  valid <- list(
    manufacturer = "Acme",
    channel_metadata = list(
      C3 = list(label = "C3", unit = "uV", type = "EEG"),
      C4 = list(label = "C4", unit = "uV", hardware_index = "2")
    )
  )
  expect_identical(
    PhysioStream:::.lsl_outlet_metadata(make_info(valid)),
    valid
  )

  extra <- valid
  extra$private_tree <- "must not be copied"
  expect_error(
    PhysioStream:::.lsl_outlet_metadata(make_info(extra)),
    "unrecognized governed"
  )
  corrupt <- make_info(valid)
  corrupt@metadata$lsl <- unname(valid)
  expect_error(
    PhysioStream:::.lsl_outlet_metadata(corrupt),
    "unique non-empty names"
  )
  duplicated <- valid
  names(duplicated)[[2L]] <- names(duplicated)[[1L]]
  corrupt <- make_info(valid)
  corrupt@metadata$lsl <- duplicated
  expect_error(
    PhysioStream:::.lsl_outlet_metadata(corrupt),
    "unique non-empty names"
  )
  wrong_order <- valid
  names(wrong_order$channel_metadata) <- c("C4", "C3")
  expect_error(
    PhysioStream:::.lsl_outlet_metadata(make_info(wrong_order)),
    "match channel order"
  )
  wrong_label <- valid
  wrong_label$channel_metadata$C3$label <- "Cz"
  expect_error(
    PhysioStream:::.lsl_outlet_metadata(make_info(wrong_label)),
    "labels differ"
  )
  unnamed_channel <- valid
  unnamed_channel$channel_metadata[[1L]] <- unname(
    unnamed_channel$channel_metadata[[1L]]
  )
  corrupt <- make_info(valid)
  corrupt@metadata$lsl <- unnamed_channel
  expect_error(
    PhysioStream:::.lsl_outlet_metadata(corrupt),
    "unique non-empty names"
  )
})

test_that("resolver arguments reject partial and malformed queries", {
  expect_error(lslResolveStreams("na", "x"), "both be NULL")
  expect_error(lslResolveStreams("name", NULL), "both be NULL")
  expect_error(lslResolveStreams(NULL, "x"), "both be NULL")
  expect_error(lslResolveStreams("name", ""), "both be NULL")
  expect_error(lslResolveStreams(minimum = 1.5), "exact integer")
  expect_error(lslResolveStreams(timeout = Inf), "finite")
})
