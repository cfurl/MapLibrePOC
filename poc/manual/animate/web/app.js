(() => {
  "use strict";

  const DEFAULT_MANIFEST_URL =
    "https://cfhydromet.com/CONUS_subset/production_areas/texas/precip/daily/pmtiles/manifests/radar_frames.json";

  // Temporary generic basemap. Replace with your production cfhydromet style URL
  // when ready. You can also override with ?style=<URL>.
  const DEFAULT_BASEMAP_STYLE =
    "https://demotiles.maplibre.org/style.json";

  const params = new URLSearchParams(window.location.search);
  const manifestUrl = params.get("manifest") || DEFAULT_MANIFEST_URL;
  const basemapStyle = params.get("style") || DEFAULT_BASEMAP_STYLE;

  const SOURCE_IDS = { A: "rain-source-a", B: "rain-source-b" };
  const LAYER_IDS = { A: "rain-layer-a", B: "rain-layer-b" };

  let manifest = null;
  let frames = [];
  let currentIndex = -1;
  let activeSlot = "A";
  let timer = null;
  let isPlaying = false;
  let isTransitioning = false;
  let playbackMs = 750;

  const playPauseBtn = document.getElementById("playPauseBtn");
  const prevBtn = document.getElementById("prevBtn");
  const nextBtn = document.getElementById("nextBtn");
  const latestBtn = document.getElementById("latestBtn");
  const speedSelect = document.getElementById("speedSelect");
  const areaLabel = document.getElementById("areaLabel");
  const dateLabel = document.getElementById("dateLabel");
  const frameLabel = document.getElementById("frameLabel");

  const protocol = new pmtiles.Protocol();
  maplibregl.addProtocol("pmtiles", protocol.tile);

  const map = new maplibregl.Map({
    container: "map",
    style: basemapStyle,
    center: [-99.3, 31.0],
    zoom: 5.2,
    attributionControl: true
  });

  map.addControl(new maplibregl.NavigationControl(), "top-right");

  function otherSlot(slot) {
    return slot === "A" ? "B" : "A";
  }

  function sourceId(slot) {
    return SOURCE_IDS[slot];
  }

  function layerId(slot) {
    return LAYER_IDS[slot];
  }

  function toPmtilesProtocolUrl(url) {
    return url.startsWith("pmtiles://") ? url : `pmtiles://${url}`;
  }

  function setStatus(frameIndex) {
    if (!manifest || !frames.length || frameIndex < 0) return;
    const frame = frames[frameIndex];
    areaLabel.textContent = `${manifest.area_id || ""} `;
    dateLabel.textContent = frame.date || frame.cycle || "";
    frameLabel.textContent = ` (${frameIndex + 1}/${frames.length})`;
  }

  function setError(message) {
    areaLabel.textContent = "";
    dateLabel.textContent = "";
    frameLabel.textContent = message;
    frameLabel.classList.add("error");
  }

  function clearError() {
    frameLabel.classList.remove("error");
  }

  function removeSlot(slot) {
    const lyr = layerId(slot);
    const src = sourceId(slot);

    if (map.getLayer(lyr)) map.removeLayer(lyr);
    if (map.getSource(src)) map.removeSource(src);
  }

  function addFrameToSlot(slot, frame, opacity) {
    removeSlot(slot);

    map.addSource(sourceId(slot), {
      type: "raster",
      url: toPmtilesProtocolUrl(frame.pmtiles),
      tileSize: 256
    });

    map.addLayer({
      id: layerId(slot),
      type: "raster",
      source: sourceId(slot),
      paint: {
        "raster-opacity": opacity,
        "raster-fade-duration": 0
      }
    });
  }

  function waitForSourceReady(slot, timeoutMs = 15000) {
    const srcId = sourceId(slot);

    return new Promise((resolve, reject) => {
      const started = Date.now();

      function cleanup() {
        map.off("sourcedata", onSourceData);
        clearInterval(interval);
      }

      function finishIfReady() {
        try {
          if (map.getSource(srcId) && map.isSourceLoaded(srcId)) {
            cleanup();
            resolve();
            return true;
          }
        } catch (_) {}
        return false;
      }

      function onSourceData(e) {
        if (e.sourceId === srcId) finishIfReady();
      }

      map.on("sourcedata", onSourceData);

      const interval = setInterval(() => {
        if (finishIfReady()) return;

        if (Date.now() - started > timeoutMs) {
          cleanup();
          reject(new Error(`Timed out waiting for ${srcId}`));
        }
      }, 100);
    });
  }

  async function showFrame(index, { immediate = false } = {}) {
    if (!frames.length || isTransitioning) return;

    clearError();
    isTransitioning = true;

    try {
      const normalized = ((index % frames.length) + frames.length) % frames.length;
      const frame = frames[normalized];

      if (currentIndex < 0 || immediate) {
        addFrameToSlot(activeSlot, frame, 1);
        await waitForSourceReady(activeSlot);
        removeSlot(otherSlot(activeSlot));
        currentIndex = normalized;
        setStatus(currentIndex);
        return;
      }

      const inactiveSlot = otherSlot(activeSlot);

      addFrameToSlot(inactiveSlot, frame, 0);
      await waitForSourceReady(inactiveSlot);

      map.setPaintProperty(layerId(activeSlot), "raster-opacity", 0);
      map.setPaintProperty(layerId(inactiveSlot), "raster-opacity", 1);

      const oldSlot = activeSlot;
      activeSlot = inactiveSlot;
      currentIndex = normalized;
      setStatus(currentIndex);

      removeSlot(oldSlot);

    } catch (err) {
      console.error(err);
      setError(`Frame load failed: ${err.message}`);
      pause();
    } finally {
      isTransitioning = false;
    }
  }

  function scheduleNextTick() {
    clearTimeout(timer);
    if (!isPlaying) return;

    timer = setTimeout(async () => {
      if (!isPlaying) return;

      const nextIndex = currentIndex + 1;

      if (nextIndex >= frames.length) {
        if (manifest?.browser_defaults?.loop === false) {
          pause();
          return;
        }
        await showFrame(0);
      } else {
        await showFrame(nextIndex);
      }

      scheduleNextTick();
    }, playbackMs);
  }

  function play() {
    if (!frames.length) return;

    isPlaying = true;
    playPauseBtn.textContent = "Pause";

    if (currentIndex === frames.length - 1) {
      showFrame(0).then(scheduleNextTick);
    } else {
      scheduleNextTick();
    }
  }

  function pause() {
    isPlaying = false;
    playPauseBtn.textContent = "Play";
    clearTimeout(timer);
    timer = null;
  }

  async function previousFrame() {
    pause();
    await showFrame(currentIndex - 1);
  }

  async function nextFrame() {
    pause();
    await showFrame(currentIndex + 1);
  }

  async function jumpToLatest() {
    pause();
    await showFrame(frames.length - 1);
  }

  async function loadManifest() {
    const response = await fetch(manifestUrl, { cache: "no-cache" });

    if (!response.ok) {
      throw new Error(
        `Manifest request failed: HTTP ${response.status} ${response.statusText}`
      );
    }

    const data = await response.json();

    if (!Array.isArray(data.frames) || data.frames.length === 0) {
      throw new Error("Manifest contains no usable frames.");
    }

    for (const frame of data.frames) {
      if (!frame.pmtiles) {
        throw new Error("A manifest frame is missing its pmtiles URL.");
      }
    }

    manifest = data;
    frames = data.frames;
    playbackMs = Number(data?.browser_defaults?.playback_ms) || 750;
    speedSelect.value = String(playbackMs);

    await showFrame(frames.length - 1, { immediate: true });
  }

  playPauseBtn.addEventListener("click", () => {
    if (isPlaying) pause();
    else play();
  });

  prevBtn.addEventListener("click", previousFrame);
  nextBtn.addEventListener("click", nextFrame);
  latestBtn.addEventListener("click", jumpToLatest);

  speedSelect.addEventListener("change", () => {
    playbackMs = Number(speedSelect.value);
    if (isPlaying) scheduleNextTick();
  });

  map.on("load", async () => {
    try {
      await loadManifest();
    } catch (err) {
      console.error(err);
      setError(err.message);
    }
  });
})();
