/* Shared webR runner for the API-209 site (assignment player + warm-up).
   Single source of truth for the webR version pin, engine lifecycle, and
   chunk execution. The engine reports an honest state machine so students
   can always tell apart: idle / starting (with detail + elapsed) / ready /
   failed. Stall display is derived from elapsed time by the consumer. */
window.API209WebR = (function () {
  "use strict";

  /* Pinned — never use /latest/ on pages that produce graded work. To
     bump: change this constant, then run the part-page test matrix in
     plans/010 before pushing. */
  var WEBR_URL = "https://webr.r-wasm.org/v0.5.4/webr.mjs";

  var webRPromise = null;
  var state = { name: "idle", detail: "", since: Date.now() };
  var listeners = [];
  var heartbeat = null;
  var options = { prewarmPackages: [], dataFiles: [], dataBase: "web-assignments/data/" };

  function snapshot() {
    return {
      name: state.name,
      detail: state.detail,
      elapsed: Date.now() - state.since
    };
  }

  function emit() {
    var snap = snapshot();
    listeners.forEach(function (fn) {
      try { fn(snap); } catch (err) { /* listener errors stay local */ }
    });
  }

  function setState(name, detail) {
    state = { name: name, detail: detail || "", since: Date.now() };
    if (name === "starting" && !heartbeat) {
      /* re-emit every 10s so consumers can show "still working / stalled"
         purely from elapsed time */
      heartbeat = setInterval(emit, 10000);
    }
    if (name !== "starting" && heartbeat) {
      clearInterval(heartbeat);
      heartbeat = null;
    }
    emit();
  }

  function configure(opts) {
    Object.keys(opts || {}).forEach(function (k) {
      if (k in options) { options[k] = opts[k]; }
    });
  }

  function onState(fn) {
    listeners.push(fn);
    fn(snapshot());
  }

  function ensure() {
    if (!webRPromise) {
      setState("starting", "downloading R (~15 MB)");
      webRPromise = import(WEBR_URL)
        .then(async function (mod) {
          var webR = new mod.WebR();
          await webR.init();
          /* Makes the standard install.packages()/library() boilerplate in
             assignment chunks work in webR; exported .Rmd keeps normal CRAN
             code for RStudio/Posit. */
          await webR.evalRVoid("webr::shim_install()");
          if (options.prewarmPackages.length) {
            setState("starting", "installing R packages (a few minutes the first time)");
            await webR.evalRVoid(
              "webr::install(c(" + options.prewarmPackages.map(function (p) {
                return '"' + String(p).replace(/[^A-Za-z0-9.]/g, "") + '"';
              }).join(",") + "))"
            );
          }
          if (options.dataFiles.length) {
            setState("starting", "loading the course data");
            try { await webR.FS.mkdir("/home/web_user/data"); } catch (err) { /* exists */ }
            for (var i = 0; i < options.dataFiles.length; i += 1) {
              var f = options.dataFiles[i];
              var res = await fetch(options.dataBase + f);
              if (!res.ok) { throw new Error("data fetch failed: " + f); }
              var buf = new Uint8Array(await res.arrayBuffer());
              await webR.FS.writeFile("/home/web_user/data/" + f, buf);
            }
          }
          setState("ready");
          return webR;
        })
        .catch(function (err) {
          webRPromise = null;
          setState("failed");
          throw err;
        });
    }
    return webRPromise;
  }

  function retry() {
    /* From failed: clean restart. From a long-stalled start: abandon the
       in-flight attempt (it becomes a no-op orphan) and start fresh. */
    webRPromise = null;
    setState("idle");
    ensure().catch(function () { /* surfaced via state */ });
  }

  async function runCode(code, opts) {
    opts = opts || {};
    var webR = await ensure();
    var shelter = await new webR.Shelter();
    try {
      var captureOpts = { withAutoprint: true };
      var wantGraphics = opts.graphics !== false && typeof OffscreenCanvas !== "undefined";
      if (wantGraphics) {
        captureOpts.captureGraphics = { width: 686, height: 430, bg: "white" };
      }
      var result;
      try {
        result = await shelter.captureR(code, captureOpts);
      } catch (firstErr) {
        if (wantGraphics) {
          result = await shelter.captureR(code, { withAutoprint: true });
        } else {
          throw firstErr;
        }
      }
      var text = result.output
        .filter(function (m) { return m.type === "stdout" || m.type === "stderr"; })
        .map(function (m) { return m.data; })
        .join("\n");
      return { text: text, images: result.images || [] };
    } finally {
      shelter.purge();
    }
  }

  async function resetSession() {
    if (!webRPromise) { return false; }
    var webR = await ensure();
    await webR.evalRVoid("rm(list = ls())");
    return true;
  }

  return {
    configure: configure,
    onState: onState,
    ensure: ensure,
    retry: retry,
    runCode: runCode,
    resetSession: resetSession,
    getState: snapshot,
    WEBR_URL: WEBR_URL
  };
})();
