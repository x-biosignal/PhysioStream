test_that("MQTT constructor is side-effect free and state excludes secrets", {
  trigger <- mqttTrigger(
    host = "localhost",
    topic = "physiostream/test/stim",
    client_id = "test-client",
    allowed_channels = "left",
    max_intensity = 10,
    intensity_unit = "mA",
    max_duration_ms = 100,
    refractory_ms = 100,
    deadman_ms = 1000,
    port = 1883L,
    tls = "disabled",
    allow_insecure_localhost = TRUE,
    username = "test-user",
    password = "unique-test-secret"
  )
  expect_s3_class(trigger, "MqttTrigger")
  expect_null(trigger$runtime$adapter)
  state_text <- paste(capture.output(str(triggerState(trigger))), collapse = "")
  expect_false(grepl("unique-test-secret|test-user", state_text))
  expect_identical(triggerState(trigger)$configuration$endpoint$retain, FALSE)
  expect_identical(triggerState(trigger)$configuration$endpoint$qos, 1L)
})

test_that("MQTT policy rejects wildcard, QoS2, and insecure remote brokers", {
  args <- list(
    host = "localhost", topic = "physiostream/test/stim",
    client_id = "client", allowed_channels = "left",
    max_intensity = 10, intensity_unit = "mA",
    max_duration_ms = 100, refractory_ms = 100, deadman_ms = 1000
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(topic = "stim/+"))),
    "canonical"
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(qos = 2))),
    "invalid value"
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(
      tls = matrix("required")
    ))),
    "exactly one"
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(
      host = "broker.example.org", tls = "disabled",
      allow_insecure_localhost = TRUE
    ))),
    "loopback"
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(
      tls = "disabled", allow_insecure_localhost = FALSE
    ))),
    "loopback"
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(
      password = "secret"
    ))),
    "requires"
  )
  expect_error(
    do.call(mqttTrigger, modifyList(args, list(clock = function() 0))),
    "clock injection"
  )
})

test_that("configured Paho capability is plain and pinned", {
  skip_if_not(mqttAvailable(initialize = TRUE), "paho-mqtt unavailable")
  info <- mqttBackendInfo()
  expect_identical(info$backend, "paho-mqtt")
  expect_identical(info$paho_mqtt_version, "2.1.0")
  expect_identical(info$mqtt_protocol, "5.0")
  expect_identical(info$callback_api, "VERSION2")
  expect_identical(info$qos, c(0L, 1L))
  expect_false(info$retain)
  expect_identical(info$network_loop, "manual")
  expect_null(PhysioStream:::.dsp_runtime_path(info))
})

test_that("MQTT v5 round-trip is canonical and receiver-deduplicated", {
  port <- Sys.getenv("PHYSIOSTREAM_MQTT_TEST_PORT", unset = "")
  skip_if(!nzchar(port), "explicit local MQTT broker is not enabled")
  skip_if_not(mqttAvailable(initialize = TRUE), "paho-mqtt unavailable")
  port <- as.integer(port)
  expect_true(is.finite(port) && port > 0L && port <= 65535L)

  mqtt <- reticulate::import("paho.mqtt.client", convert = FALSE)
  reticulate::py_run_string(paste(
    "def _physiostream_test_subscriber_open_v1(mqtt, host, port, topic):",
    "    import time",
    "    state = {'connected': False, 'subscribed': False, 'messages': []}",
    "    def on_connect(client, userdata, flags, reason_code, properties):",
    "        if bool(getattr(reason_code, 'is_failure', False)):",
    "            raise RuntimeError('test subscriber connect rejected')",
    "        state['connected'] = True",
    "        client.subscribe(str(topic), qos=1)",
    "    def on_subscribe(client, userdata, mid, reason_codes, properties):",
    "        state['subscribed'] = True",
    "    def on_message(client, userdata, message):",
    "        state['messages'].append({",
    "            'payload': message.payload.decode('utf-8'),",
    "            'qos': int(message.qos), 'retain': bool(message.retain)})",
    "    client = mqtt.Client(",
    "        callback_api_version=mqtt.CallbackAPIVersion.VERSION2,",
    "        client_id='physiostream-test-subscriber', protocol=mqtt.MQTTv5)",
    "    client.on_connect = on_connect",
    "    client.on_subscribe = on_subscribe",
    "    client.on_message = on_message",
    "    client._physiostream_test_state = state",
    "    client.connect(str(host), int(port), keepalive=30,",
    "                   clean_start=mqtt.MQTT_CLEAN_START_FIRST_ONLY)",
    "    deadline = time.monotonic() + 3.0",
    "    while not state['subscribed']:",
    "        if time.monotonic() >= deadline:",
    "            raise TimeoutError('test subscriber timeout')",
    "        client.loop(timeout=0.05)",
    "    return client",
    "",
    "def _physiostream_test_subscriber_receive_v1(client, count):",
    "    import time",
    "    state = client._physiostream_test_state",
    "    deadline = time.monotonic() + 3.0",
    "    while len(state['messages']) < int(count):",
    "        if time.monotonic() >= deadline:",
    "            raise TimeoutError('test message timeout')",
    "        client.loop(timeout=0.05)",
    "    return list(state['messages'])",
    "",
    "def _physiostream_test_publish_duplicate_v1(client, topic, payload):",
    "    import time",
    "    info = client.publish(str(topic), str(payload), qos=1, retain=False)",
    "    deadline = time.monotonic() + 3.0",
    "    while not info.is_published():",
    "        if time.monotonic() >= deadline:",
    "            raise TimeoutError('test duplicate publish timeout')",
    "        client.loop(timeout=0.05)",
    "    return None",
    "",
    "def _physiostream_test_subscriber_close_v1(client):",
    "    client.disconnect()",
    "    client.loop(timeout=0.05)",
    "    return None",
    sep = "\n"
  ))
  topic <- paste0("physiostream/test/", Sys.getpid(), "/stim")
  subscriber <- reticulate::py[[
    "_physiostream_test_subscriber_open_v1"
  ]](mqtt, "127.0.0.1", port, topic)
  on.exit(try(
    reticulate::py[["_physiostream_test_subscriber_close_v1"]](subscriber),
    silent = TRUE
  ), add = TRUE)

  trigger <- mqttTrigger(
    host = "127.0.0.1", port = port, topic = topic,
    client_id = paste0("physiostream-publisher-", Sys.getpid()),
    allowed_channels = "left", max_intensity = 10,
    intensity_unit = "mA", max_duration_ms = 100,
    refractory_ms = 100, deadman_ms = 1000,
    tls = "disabled", allow_insecure_localhost = TRUE
  )
  triggerOpen(trigger)
  on.exit(try(triggerClose(trigger), silent = TRUE), add = TRUE)
  armTrigger(trigger, "session", now_ns = 0)
  receipt <- sendStim(trigger, 1, "left", 10, "command-1", now_ns = 0)
  expect_identical(receipt$status, "acknowledged")

  first <- reticulate::py[[
    "_physiostream_test_subscriber_receive_v1"
  ]](subscriber, 1L)
  if (reticulate::is_py_object(first)) {
    first <- reticulate::py_to_r(first)
  }
  expect_identical(first[[1L]]$payload, receipt$payload)
  expect_identical(first[[1L]]$qos, 1L)
  expect_false(first[[1L]]$retain)

  reticulate::py[["_physiostream_test_publish_duplicate_v1"]](
    subscriber, topic, receipt$payload
  )
  messages <- reticulate::py[[
    "_physiostream_test_subscriber_receive_v1"
  ]](subscriber, 2L)
  if (reticulate::is_py_object(messages)) {
    messages <- reticulate::py_to_r(messages)
  }
  command_ids <- vapply(
    messages,
    function(message) jsonlite::fromJSON(message$payload)$command_id,
    character(1)
  )
  expect_identical(command_ids, c("command-1", "command-1"))
  expect_identical(unique(command_ids), "command-1")

  qos0_trigger <- mqttTrigger(
    host = "127.0.0.1", port = port, topic = topic,
    client_id = paste0("physiostream-publisher-qos0-", Sys.getpid()),
    allowed_channels = "left", max_intensity = 10,
    intensity_unit = "mA", max_duration_ms = 100,
    refractory_ms = 100, deadman_ms = 1000, qos = 0,
    tls = "disabled", allow_insecure_localhost = TRUE
  )
  triggerOpen(qos0_trigger)
  on.exit(try(triggerClose(qos0_trigger), silent = TRUE), add = TRUE)
  armTrigger(qos0_trigger, "session-qos0", now_ns = 0)
  qos0_receipt <- sendStim(
    qos0_trigger, 1, "left", 10, "command-qos0", now_ns = 0
  )
  expect_identical(qos0_receipt$status, "acknowledged")
  expect_identical(qos0_receipt$ack_code, "mqtt_sent")
  all_messages <- reticulate::py[[
    "_physiostream_test_subscriber_receive_v1"
  ]](subscriber, 3L)
  if (reticulate::is_py_object(all_messages)) {
    all_messages <- reticulate::py_to_r(all_messages)
  }
  expect_identical(all_messages[[3L]]$payload, qos0_receipt$payload)
  expect_identical(all_messages[[3L]]$qos, 0L)
  expect_false(all_messages[[3L]]$retain)
})
