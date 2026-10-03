.mqtt_capability_schema <- "1.0.0"

.mqtt_scalar_secret <- function(x, name, allow_empty = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  valid <- !is.factor(x) && !is.object(x) && is.character(x) &&
    is.null(dim(x)) && length(x) == 1L &&
    !is.na(x) && nchar(x, type = "bytes") <= 4096L &&
    (allow_empty || nzchar(x))
  if (!valid) {
    .trigger_abort(
      sprintf("`%s` must be NULL or one bounded string", name),
      "PhysioStream_trigger_validation_error"
    )
  }
  enc2utf8(x)
}

.mqtt_host <- function(host) {
  host <- .trigger_string(host, "host", max_bytes = 253L)
  if (grepl("[/?#[:space:]]", host, perl = TRUE)) {
    .trigger_abort(
      "`host` must be a hostname or IP literal without URL syntax",
      "PhysioStream_trigger_validation_error"
    )
  }
  host
}

.mqtt_topic <- function(topic) {
  topic <- .trigger_string(topic, "topic", max_bytes = 65535L)
  utf8 <- iconv(topic, from = "UTF-8", to = "UTF-8", sub = NA)
  if (is.na(utf8) || grepl("[+#]", topic, perl = TRUE) ||
      startsWith(topic, "/") || endsWith(topic, "/") ||
      grepl("//", topic, fixed = TRUE)) {
    .trigger_abort(
      "`topic` must be a concrete canonical MQTT publish topic",
      "PhysioStream_trigger_validation_error"
    )
  }
  topic
}

.mqtt_loopback_host <- function(host) {
  tolower(host) %in% c("localhost", "127.0.0.1", "::1", "[::1]")
}

.mqtt_make_module <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    .trigger_abort(
      "MQTT requires the suggested package `reticulate`",
      "PhysioStream_trigger_unavailable"
    )
  }
  module <- tryCatch(
    reticulate::import("paho.mqtt.client", convert = TRUE),
    error = function(e) {
      .trigger_abort(
        "configured Python cannot import pinned paho-mqtt",
        "PhysioStream_trigger_unavailable"
      )
    }
  )
  reticulate::py_run_string(
    paste(
      "def _physiostream_mqtt_open_v1(mqtt, host, port, client_id,",
      "                                  tls_required, username, password,",
      "                                  timeout_s):",
      "    import time",
      "    state = {'connected': False, 'failure': False}",
      "    def on_connect(client, userdata, flags, reason_code, properties):",
      "        state['failure'] = bool(getattr(reason_code, 'is_failure', False))",
      "        state['connected'] = not state['failure']",
      "    client = mqtt.Client(",
      "        callback_api_version=mqtt.CallbackAPIVersion.VERSION2,",
      "        client_id=str(client_id), protocol=mqtt.MQTTv5)",
      "    client.on_connect = on_connect",
      "    if username is not None:",
      "        client.username_pw_set(str(username),",
      "                               None if password is None else str(password))",
      "    if bool(tls_required):",
      "        client.tls_set()",
      "    rc = client.connect(str(host), int(port), keepalive=30,",
      "                        clean_start=mqtt.MQTT_CLEAN_START_FIRST_ONLY)",
      "    if int(rc) != int(mqtt.MQTT_ERR_SUCCESS):",
      "        raise RuntimeError('MQTT connect invocation failed')",
      "    deadline = time.monotonic() + float(timeout_s)",
      "    while not state['connected'] and not state['failure']:",
      "        if time.monotonic() >= deadline:",
      "            raise TimeoutError('MQTT connect acknowledgement timeout')",
      "        rc = client.loop(timeout=min(0.05, max(0.0, deadline-time.monotonic())))",
      "        if int(rc) != int(mqtt.MQTT_ERR_SUCCESS):",
      "            raise RuntimeError('MQTT network loop failed')",
      "    if state['failure']:",
      "        raise RuntimeError('MQTT broker rejected connection')",
      "    return client",
      "",
      "def _physiostream_mqtt_publish_v1(mqtt, client, topic, payload, qos,",
      "                                     timeout_s):",
      "    import time",
      "    info = client.publish(str(topic), payload=str(payload),",
      "                          qos=int(qos), retain=False)",
      "    if int(info.rc) != int(mqtt.MQTT_ERR_SUCCESS):",
      "        raise RuntimeError('MQTT publish invocation failed')",
      "    deadline = time.monotonic() + float(timeout_s)",
      "    while not info.is_published():",
      "        if time.monotonic() >= deadline:",
      "            raise TimeoutError('MQTT publish acknowledgement timeout')",
      "        rc = client.loop(timeout=min(0.05, max(0.0, deadline-time.monotonic())))",
      "        if int(rc) != int(mqtt.MQTT_ERR_SUCCESS):",
      "            raise RuntimeError('MQTT publish network loop failed')",
      "    return {'ok': True,",
      "            'ack_code': 'mqtt_puback' if int(qos) == 1 else 'mqtt_sent',",
      "            'mid': int(info.mid), 'qos': int(qos), 'retain': False}",
      "",
      "def _physiostream_mqtt_close_v1(mqtt, client):",
      "    import time",
      "    try:",
      "        client.disconnect()",
      "        deadline = time.monotonic() + 0.25",
      "        while time.monotonic() < deadline:",
      "            rc = client.loop(timeout=0.01)",
      "            if int(rc) != int(mqtt.MQTT_ERR_SUCCESS):",
      "                break",
      "    finally:",
      "        return None",
      sep = "\n"
    )
  )
  module
}

.mqtt_backend_info <- function(module) {
  metadata <- reticulate::import("importlib.metadata", convert = TRUE)
  config <- reticulate::py_config()
  python_version <- strsplit(
    as.character(config$version_string), " ", fixed = TRUE
  )[[1L]][[1L]]
  list(
    backend = "paho-mqtt",
    python = as.character(config$python),
    python_version = python_version,
    reticulate_version = as.character(utils::packageVersion("reticulate")),
    paho_mqtt_version = as.character(metadata$version("paho-mqtt")),
    mqtt_protocol = "5.0",
    callback_api = "VERSION2",
    qos = c(0L, 1L),
    retain = FALSE,
    network_loop = "manual",
    capability_schema = .mqtt_capability_schema
  )
}

.mqtt_open_adapter <- function(config, credentials) {
  module <- .mqtt_make_module()
  client <- tryCatch(
    reticulate::py[["_physiostream_mqtt_open_v1"]](
      module,
      config$host,
      as.integer(config$port),
      config$client_id,
      identical(config$tls, "required"),
      credentials$username,
      credentials$password,
      config$connect_timeout_ms / 1000
    ),
    error = function(e) {
      .trigger_abort(
        "MQTT transport failed to connect",
        "PhysioStream_trigger_transport_error"
      )
    }
  )
  adapter <- new.env(parent = emptyenv())
  adapter$client <- client
  adapter$close <- function() {
    tryCatch(
      reticulate::py[["_physiostream_mqtt_close_v1"]](module, client),
      error = function(e) {
        .trigger_abort(
          "MQTT transport failed to close",
          "PhysioStream_trigger_transport_error"
        )
      }
    )
    invisible(TRUE)
  }
  adapter$publish <- function(payload) {
    tryCatch(
      {
        result <- reticulate::py[["_physiostream_mqtt_publish_v1"]](
          module, client, config$topic, payload, as.integer(config$qos),
          config$publish_timeout_ms / 1000
        )
        if (reticulate::is_py_object(result)) {
          reticulate::py_to_r(result)
        } else {
          result
        }
      },
      error = function(e) {
        .trigger_abort(
          "MQTT publish acknowledgement is unknown",
          "PhysioStream_trigger_transport_error"
        )
      }
    )
  }
  adapter
}

#' Test or describe the configured MQTT backend
#'
#' The conservative availability probe does not select or install Python.
#' Initialization is explicit and uses only the interpreter already configured
#' for reticulate.
#'
#' @param initialize Whether the configured Python interpreter may be
#'   initialized to verify pinned Paho.
#' @return `mqttAvailable()` returns one logical value.
#'   `mqttBackendInfo()` returns a plain capability list.
#' @examples
#' # Reports FALSE unless the pinned Paho MQTT backend is installed.
#' mqttAvailable()
#' @export
mqttAvailable <- function(initialize = FALSE) {
  initialize <- .trigger_logical(initialize, "initialize")
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }
  if (!initialize) {
    if (!isTRUE(reticulate::py_available(initialize = FALSE))) {
      return(FALSE)
    }
    return(isTRUE(tryCatch(
      !is.null(
        reticulate::import(
          "importlib.util", convert = TRUE
        )$find_spec("paho.mqtt.client")
      ),
      error = function(e) FALSE
    )))
  }
  isTRUE(tryCatch({
    .mqtt_make_module()
    TRUE
  }, error = function(e) FALSE))
}

#' @rdname mqttAvailable
#' @export
mqttBackendInfo <- function() {
  .mqtt_backend_info(.mqtt_make_module())
}

#' Construct a governed MQTT stimulation-command trigger
#'
#' Uses MQTT v5, Paho callback API VERSION2, a caller-thread manual network
#' loop, QoS 0 or 1, and `retain = FALSE`. QoS 1 can duplicate a command, so the
#' receiver must deduplicate exact command IDs. No exactly-once or physical
#' delivery claim is made.
#'
#' TLS is required by default. Plaintext is restricted to an explicitly
#' acknowledged loopback-only test broker.
#'
#' @inheritParams loopbackTrigger
#' @param host Broker hostname or IP literal.
#' @param topic Concrete publish topic without wildcards.
#' @param client_id Bounded MQTT client identifier.
#' @param port Integer broker port.
#' @param qos Exact MQTT QoS, 0 or 1.
#' @param tls Exact `"required"` or `"disabled"`.
#' @param allow_insecure_localhost Permit plaintext only for a loopback host.
#' @param username Optional broker username, excluded from portable state.
#' @param password Optional broker password, excluded from portable state.
#' @param connect_timeout_ms Positive bounded connect timeout.
#' @param publish_timeout_ms Positive bounded acknowledgement timeout.
#' @return A closed, disarmed `MqttTrigger`.
#' @examples
#' \dontrun{
#' # Requires a reachable MQTT broker; see loopbackTrigger() for an
#' # offline equivalent with the same interlocks.
#' trigger <- mqttTrigger(host = "127.0.0.1", port = 1883,
#'                        topic = "stim/commands",
#'                        allowed_channels = "left", max_intensity = 20,
#'                        intensity_unit = "mA", max_duration_ms = 500,
#'                        refractory_ms = 0, deadman_ms = 1000)
#' }
#' @export
mqttTrigger <- function(
    host,
    topic,
    client_id,
    allowed_channels,
    max_intensity,
    intensity_unit,
    max_duration_ms,
    refractory_ms,
    deadman_ms,
    port = 8883L,
    qos = 1L,
    tls = c("required", "disabled"),
    allow_insecure_localhost = FALSE,
    username = NULL,
    password = NULL,
    connect_timeout_ms = 5000,
    publish_timeout_ms = 1000,
    audit_capacity = 4096L,
    clock = NULL) {
  if (!is.null(clock)) {
    .trigger_abort(
      "clock injection is available only for loopback triggers",
      "PhysioStream_trigger_validation_error"
    )
  }
  host <- .mqtt_host(host)
  topic <- .mqtt_topic(topic)
  client_id <- .trigger_id(client_id, "client_id")
  port <- .trigger_scalar(
    port, "port", lower = 1, upper = 65535, integer = TRUE
  )
  qos <- .trigger_scalar(
    qos, "qos", lower = 0, upper = 1, integer = TRUE
  )
  if (missing(tls)) {
    tls <- "required"
  }
  tls <- .trigger_enum(tls, c("required", "disabled"), "tls")
  allow_insecure_localhost <- .trigger_logical(
    allow_insecure_localhost, "allow_insecure_localhost"
  )
  if (identical(tls, "disabled") &&
      (!allow_insecure_localhost || !.mqtt_loopback_host(host))) {
    .trigger_abort(
      "plaintext MQTT is permitted only for an explicitly allowed loopback broker",
      "PhysioStream_trigger_validation_error"
    )
  }
  username <- .mqtt_scalar_secret(username, "username")
  password <- .mqtt_scalar_secret(password, "password", allow_empty = TRUE)
  if (!is.null(password) && is.null(username)) {
    .trigger_abort(
      "`password` requires `username`",
      "PhysioStream_trigger_validation_error"
    )
  }
  connect_timeout_ms <- .trigger_scalar(
    connect_timeout_ms, "connect_timeout_ms", lower = 0,
    lower_open = TRUE, upper = 60000
  )
  publish_timeout_ms <- .trigger_scalar(
    publish_timeout_ms, "publish_timeout_ms", lower = 0,
    lower_open = TRUE, upper = 60000
  )
  endpoint <- list(
    host = host,
    port = port,
    topic = topic,
    client_id = client_id,
    qos = qos,
    tls = tls,
    retain = FALSE
  )
  configuration <- .trigger_configuration(
    backend = "mqtt",
    transport = "mqtt",
    allowed_channels = allowed_channels,
    max_intensity = max_intensity,
    intensity_unit = intensity_unit,
    max_duration_ms = max_duration_ms,
    refractory_ms = refractory_ms,
    deadman_ms = deadman_ms,
    audit_capacity = audit_capacity,
    endpoint = endpoint
  )
  runtime_config <- c(
    endpoint,
    list(
      connect_timeout_ms = connect_timeout_ms,
      publish_timeout_ms = publish_timeout_ms
    )
  )
  .trigger_new(
    "MqttTrigger", configuration,
    runtime_config = runtime_config,
    credentials = list(username = username, password = password)
  )
}
