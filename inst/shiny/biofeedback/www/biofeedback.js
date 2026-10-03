(function () {
  "use strict";

  const palette = [
    "#006d77", "#d1495b", "#3a7d44", "#7b2cbf",
    "#d97706", "#2f5aa8", "#0081a7", "#9c2c77"
  ];
  const runtime = {
    latestFrame: null,
    lastRenderedId: -1,
    renderedFrames: 0,
    paused: false,
    visibleTraces: null,
    gain: 1,
    windowSeconds: 10,
    threshold: null,
    videoSeeks: 0,
    pendingVideo: null
  };

  function canvasContext(id) {
    const canvas = document.getElementById(id);
    if (!canvas) return null;
    const ratio = window.devicePixelRatio || 1;
    const width = Math.max(1, Math.round(canvas.clientWidth * ratio));
    const height = Math.max(1, Math.round(canvas.clientHeight * ratio));
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width;
      canvas.height = height;
    }
    const context = canvas.getContext("2d");
    context.setTransform(ratio, 0, 0, ratio, 0, 0);
    return {
      canvas: canvas,
      context: context,
      width: canvas.clientWidth,
      height: canvas.clientHeight
    };
  }

  function finite(value) {
    return typeof value === "number" && Number.isFinite(value);
  }

  function traceRange(trace) {
    let minimum = Infinity;
    let maximum = -Infinity;
    const gain = finite(trace.gain) ? trace.gain * runtime.gain : runtime.gain;
    (trace.values || []).forEach(function (value) {
      const scaled = value * gain;
      minimum = Math.min(minimum, scaled);
      maximum = Math.max(maximum, scaled);
    });
    if (!finite(minimum) || !finite(maximum)) return [-1, 1];
    if (minimum === maximum) {
      const margin = Math.max(1, Math.abs(minimum) * 0.1);
      return [minimum - margin, maximum + margin];
    }
    const margin = (maximum - minimum) * 0.08;
    return [minimum - margin, maximum + margin];
  }

  function drawScope(frame) {
    const surface = canvasContext("scope-canvas");
    if (!surface) return;
    const ctx = surface.context;
    const width = surface.width;
    const height = surface.height;
    ctx.clearRect(0, 0, width, height);
    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, width, height);

    const traces = Object.entries(frame.traces || {})
      .filter(function (entry) {
        return runtime.visibleTraces === null ||
          runtime.visibleTraces.includes(entry[0]);
      })
      .map(function (entry) { return entry[1]; });
    if (!traces.length || !finite(frame.signal_time)) {
      ctx.fillStyle = "#59656d";
      ctx.font = "13px Segoe UI, Arial, sans-serif";
      ctx.fillText("Waiting for samples", 18, 30);
      return;
    }
    const laneHeight = height / traces.length;
    const rightTime = frame.signal_time;
    const leftTime = rightTime - runtime.windowSeconds;
    ctx.font = "11px Segoe UI, Arial, sans-serif";

    traces.forEach(function (trace, index) {
      const top = index * laneHeight;
      const bottom = top + laneHeight;
      const center = top + laneHeight / 2;
      const color = palette[index % palette.length];
      const range = traceRange(trace);
      const span = range[1] - range[0];
      const gain = finite(trace.gain) ? trace.gain * runtime.gain : runtime.gain;

      ctx.strokeStyle = "#e1e7ea";
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(0, bottom - 0.5);
      ctx.lineTo(width, bottom - 0.5);
      ctx.stroke();

      ctx.fillStyle = "#59656d";
      ctx.fillText(
        (trace.channel || trace.name) + "  " + (trace.unit || ""),
        10,
        top + 16
      );

      const threshold = finite(runtime.threshold)
        ? runtime.threshold
        : trace.threshold;
      if (finite(threshold)) {
        const y = bottom - ((threshold - range[0]) / span) * laneHeight;
        ctx.strokeStyle = "#b35c00";
        ctx.setLineDash([4, 4]);
        ctx.beginPath();
        ctx.moveTo(0, y);
        ctx.lineTo(width, y);
        ctx.stroke();
        ctx.setLineDash([]);
      }

      const timestamps = trace.timestamps || [];
      const values = trace.values || [];
      ctx.strokeStyle = color;
      ctx.lineWidth = 1.4;
      ctx.beginPath();
      let started = false;
      for (let i = 0; i < timestamps.length; i += 1) {
        const time = timestamps[i];
        if (time < leftTime || time > rightTime || !finite(values[i])) continue;
        const x = ((time - leftTime) / runtime.windowSeconds) * width;
        const scaled = values[i] * gain;
        const y = bottom - ((scaled - range[0]) / span) * laneHeight;
        if (!started) {
          ctx.moveTo(x, y);
          started = true;
        } else {
          ctx.lineTo(x, y);
        }
      }
      if (started) ctx.stroke();

      ctx.strokeStyle = "#9aabb3";
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(width - 1, top);
      ctx.lineTo(width - 1, bottom);
      ctx.stroke();
      if (center < 0) return;
    });
  }

  function drawTarget(frame) {
    const surface = canvasContext("target-canvas");
    if (!surface) return;
    const ctx = surface.context;
    const width = surface.width;
    const height = surface.height;
    ctx.clearRect(0, 0, width, height);
    ctx.fillStyle = "#f3f6f7";
    ctx.fillRect(0, 0, width, height);

    const targets = Object.values(frame.targets || {});
    const target = targets.length ? targets[0] : null;
    const targetValue = document.getElementById("target-value");
    if (!target || !(target.values || []).length) {
      if (targetValue) targetValue.textContent = "--";
      return;
    }
    const value = target.values[target.values.length - 1];
    const range = target.target_range && target.target_range.length === 2
      ? target.target_range
      : [value - Math.max(1, Math.abs(value)), value + Math.max(1, Math.abs(value))];
    const normalized = Math.max(0, Math.min(1, (value - range[0]) / (range[1] - range[0])));
    ctx.fillStyle = "#d6dde1";
    ctx.fillRect(22, height / 2 - 14, width - 44, 28);
    ctx.fillStyle = "#006d77";
    ctx.fillRect(22, height / 2 - 14, (width - 44) * normalized, 28);
    if (finite(target.threshold)) {
      const thresholdX = 22 + (width - 44) *
        Math.max(0, Math.min(1, (target.threshold - range[0]) / (range[1] - range[0])));
      ctx.fillStyle = "#b35c00";
      ctx.fillRect(thresholdX - 1, height / 2 - 22, 2, 44);
    }
    if (targetValue) {
      targetValue.textContent = value.toFixed(3) + " " + (target.unit || "");
    }
  }

  function renderLegend(frame) {
    const legend = document.getElementById("trace-legend");
    if (!legend) return;
    legend.replaceChildren();
    Object.entries(frame.traces || {})
      .filter(function (entry) {
        return runtime.visibleTraces === null ||
          runtime.visibleTraces.includes(entry[0]);
      })
      .slice(0, 8)
      .forEach(function (entry, index) {
      const trace = entry[1];
      const item = document.createElement("span");
      item.className = "legend-item";
      const swatch = document.createElement("span");
      swatch.className = "legend-swatch";
      swatch.style.backgroundColor = palette[index % palette.length];
      const label = document.createElement("span");
      label.textContent = trace.channel || trace.name;
      item.appendChild(swatch);
      item.appendChild(label);
      legend.appendChild(item);
      });
  }

  function updateStatus(frame) {
    const diagnostics = frame.diagnostics || {};
    const pipeline = diagnostics.pipeline || {};
    const source = diagnostics.source || {};
    const latency = document.getElementById("latency-value");
    const loss = document.getElementById("loss-value");
    const frames = document.getElementById("frame-value");
    if (latency) {
      latency.textContent = finite(pipeline.latest_end_to_end_ms)
        ? pipeline.latest_end_to_end_ms.toFixed(1) + " ms"
        : "--";
    }
    if (loss) {
      const dropped = finite(pipeline.dropped_samples) ? pipeline.dropped_samples : 0;
      const overwritten = finite(source.overwritten) ? source.overwritten : 0;
      loss.textContent = String(dropped + overwritten);
    }
    if (frames) frames.textContent = String(runtime.renderedFrames);
  }

  function syncVideo(frame) {
    const config = window.PHYSIOSTREAM_VIDEO;
    const video = document.getElementById("physiostream-video");
    if (!config || !video || !finite(frame.signal_time)) return;
    const sync = config.sync;
    if (frame.clock_domain !== sync.clock_domain) {
      const status = document.getElementById("scope-status");
      if (status) status.textContent = "Video clock mismatch";
      return;
    }
    const desired = sync.video_origin +
      sync.rate * (frame.signal_time - sync.signal_origin);
    if (!finite(desired) || config.kind === "webcam") {
      acknowledgeVideo(frame, desired, null);
      return;
    }
    if (video.readyState < 1 || !finite(video.duration)) return;
    const bounded = Math.max(0, Math.min(video.duration, desired));
    const driftFrames = (video.currentTime - bounded) * sync.frame_rate;
    if (Math.abs(driftFrames) > 1) {
      runtime.pendingVideo = { frame: frame, desired: bounded };
      video.currentTime = bounded;
      runtime.videoSeeks += 1;
      video.playbackRate = 1;
      return;
    } else if (!video.paused) {
      video.playbackRate = Math.max(0.95, Math.min(1.05, 1 - driftFrames * 0.005));
    } else {
      video.playbackRate = 1;
    }
    acknowledgeVideo(frame, bounded, driftFrames);
  }

  function acknowledgeVideo(frame, desired, driftFrames) {
    const video = document.getElementById("physiostream-video");
    const drift = document.getElementById("video-drift");
    if (drift) {
      drift.textContent = finite(driftFrames) ? driftFrames.toFixed(2) + " fr" : "--";
    }
    if (window.Shiny && video) {
      window.Shiny.setInputValue(
        "physiostream_video_ack",
        {
          frame_id: frame.frame_id,
          desired_time: finite(desired) ? desired : null,
          actual_time: finite(video.currentTime) ? video.currentTime : null,
          drift_frames: finite(driftFrames) ? driftFrames : null,
          seeks: runtime.videoSeeks,
          ready_state: video.readyState
        },
        { priority: "event" }
      );
    }
  }

  function renderLatest() {
    const frame = runtime.latestFrame;
    if (!frame || runtime.paused || frame.frame_id === runtime.lastRenderedId) return;
    runtime.lastRenderedId = frame.frame_id;
    runtime.renderedFrames += 1;
    drawScope(frame);
    drawTarget(frame);
    renderLegend(frame);
    updateStatus(frame);
    syncVideo(frame);
    if (window.Shiny) {
      window.Shiny.setInputValue(
        "physiostream_render_ack",
        {
          frame_id: frame.frame_id,
          rendered_frames: runtime.renderedFrames,
          retained_frames: runtime.latestFrame ? 1 : 0
        },
        { priority: "event" }
      );
    }
  }

  function animate() {
    renderLatest();
    window.requestAnimationFrame(animate);
  }

  document.addEventListener("DOMContentLoaded", function () {
    const camera = document.getElementById("start-webcam");
    const video = document.getElementById("physiostream-video");
    if (camera && video) {
      camera.addEventListener("click", function () {
        if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
          document.getElementById("scope-status").textContent = "Camera unavailable";
          return;
        }
        navigator.mediaDevices.getUserMedia({ video: true, audio: false })
          .then(function (stream) {
            video.srcObject = stream;
            video.play().catch(function () {
              document.getElementById("scope-status").textContent =
                "Camera playback blocked";
            });
          })
          .catch(function () {
            document.getElementById("scope-status").textContent = "Camera denied";
          });
      });
    }
    if (video) {
      video.addEventListener("seeked", function () {
        const pending = runtime.pendingVideo;
        if (!pending) return;
        runtime.pendingVideo = null;
        const sync = window.PHYSIOSTREAM_VIDEO.sync;
        const driftFrames =
          (video.currentTime - pending.desired) * sync.frame_rate;
        acknowledgeVideo(pending.frame, pending.desired, driftFrames);
      });
      video.addEventListener("error", function () {
        const status = document.getElementById("scope-status");
        if (status) status.textContent = "Video unavailable";
      });
    }
    window.requestAnimationFrame(animate);
  });

  if (window.Shiny) {
    window.Shiny.addCustomMessageHandler("physiostream-frame", function (frame) {
      if (!frame || !finite(frame.frame_id)) return;
      if (runtime.latestFrame && frame.frame_id <= runtime.latestFrame.frame_id) return;
      runtime.latestFrame = frame;
    });
    window.Shiny.addCustomMessageHandler("physiostream-controls", function (controls) {
      runtime.paused = Boolean(controls.paused);
      runtime.visibleTraces = Array.isArray(controls.visible_traces)
        ? controls.visible_traces
        : null;
      runtime.gain = finite(controls.gain) ? controls.gain : 1;
      runtime.windowSeconds = finite(controls.window) ? controls.window : 10;
      runtime.threshold = finite(controls.threshold) ? controls.threshold : null;
      if (!runtime.paused) runtime.lastRenderedId = -1;
    });
    window.Shiny.addCustomMessageHandler("physiostream-error", function () {
      const status = document.getElementById("scope-status");
      if (status) status.textContent = "Stopped";
    });
  }
}());
