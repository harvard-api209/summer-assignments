/* In-browser player for the API-209 coding assignments.

   Each part page sets window.API209_PART = {
     number, source, storageKey, exportBase,
     dataFiles: [...], prewarmPackages: [...]
   } before loading this script (after webr-runner.js). The player fetches
   the web-adapted assignment from docs/web-assignments/, renders prose as
   HTML, turns R chunks into editable run-in-the-browser editors (webR via
   the shared API209WebR runner), autosaves to localStorage, and exports a
   completed .Rmd for Canvas.

   The parsing/export core (parseRmd / buildRmdFrom) is pure and exported
   for node tests — its round-trip invariant is what guarantees browser
   students submit the same document as Posit Cloud students. */
(function () {
  "use strict";

  /* ===================== pure core (node-testable) ===================== */

  function parseRmd(text) {
    var lines = text.replace(/\r\n/g, "\n").split("\n");
    var i = 0;
    var yaml = [];
    if (lines[0] === "---") {
      i = 1;
      while (i < lines.length && lines[i] !== "---") {
        yaml.push(lines[i]);
        i += 1;
      }
      i += 1;
    }
    var blocks = [];
    var prose = [];
    var editableCount = 0;
    var answerCount = 0;

    function flushProse() {
      if (!prose.length) { return; }
      var items = prose.map(function (line) {
        if (/^\[Write your .*here\.\]$/.test(line)) {
          var idx = answerCount;
          answerCount += 1;
          return { answerIdx: idx, placeholder: line };
        }
        return { text: line };
      });
      blocks.push({ type: "prose", items: items });
      prose = [];
    }

    while (i < lines.length) {
      var line = lines[i];
      var fence = /^```(\{r[^}]*\}|[a-z]*)\s*$/.exec(line);
      if (fence) {
        flushProse();
        var header = line;
        var body = [];
        i += 1;
        while (i < lines.length && lines[i] !== "```") {
          body.push(lines[i]);
          i += 1;
        }
        i += 1;
        var isR = header.indexOf("```{r") === 0;
        var hidden = isR && /include\s*=\s*FALSE/.test(header);
        var block = {
          type: isR ? "chunk" : "static",
          header: header,
          body: body.join("\n"),
          hidden: hidden
        };
        if (isR && !hidden) {
          block.editIdx = editableCount;
          editableCount += 1;
        }
        blocks.push(block);
      } else {
        prose.push(line);
        i += 1;
      }
    }
    flushProse();
    return {
      yaml: yaml.join("\n"),
      blocks: blocks,
      editableCount: editableCount,
      answerCount: answerCount
    };
  }

  function validateParse(parsed, raw) {
    var problems = [];
    if (!parsed.yaml || parsed.yaml.indexOf("title:") === -1) {
      problems.push("missing YAML header");
    }
    var fences = (raw.match(/^```/gm) || []).length;
    if (fences % 2 !== 0) {
      problems.push("unbalanced code fences");
    }
    if (!parsed.blocks.some(function (b) { return b.type === "chunk"; })) {
      problems.push("no R chunks found");
    }
    return problems;
  }

  /* Name sanitizers: a newline or stray quote in the name must never
     corrupt the exported YAML, and filesystem-hostile characters must
     never reach the download filename. Display keeps what they typed. */
  function safeName(raw) {
    return String(raw).replace(/[\r\n"\\]/g, " ").replace(/\s+/g, " ").trim();
  }

  function safeFilePart(raw) {
    return safeName(raw).replace(/[\/:*?<>|#%&{}]/g, "-");
  }

  function buildRmdFrom(parsed, state, name) {
    var yaml = parsed.yaml;
    var clean = safeName(name || "");
    if (clean) {
      yaml = yaml.replace('author: "YOUR NAME"', 'author: "' + clean + '"');
    }
    var out = ["---", yaml, "---"];
    parsed.blocks.forEach(function (block) {
      if (block.type === "chunk" || block.type === "static") {
        var body = block.body;
        if (block.editIdx !== undefined && state.chunks[block.editIdx] !== undefined) {
          body = state.chunks[block.editIdx];
        }
        out.push(block.header);
        if (body.length) { out.push(body); }
        out.push("```");
      } else {
        block.items.forEach(function (item) {
          if (item.answerIdx !== undefined) {
            var ans = (state.answers[item.answerIdx] || "").trim();
            out.push(ans ? ans : item.placeholder);
          } else {
            out.push(item.text);
          }
        });
      }
    });
    return out.join("\n") + "\n";
  }

  var internals = {
    parseRmd: parseRmd,
    buildRmdFrom: buildRmdFrom,
    validateParse: validateParse,
    safeName: safeName,
    safeFilePart: safeFilePart
  };
  if (typeof module !== "undefined" && module.exports) {
    module.exports = internals;
  }
  if (typeof window === "undefined") {
    return; /* node test environment: pure core only */
  }
  window.api209PlayerInternals = internals;

  /* ===================== browser application ===================== */

  var CONFIG = window.API209_PART;
  var root = document.getElementById("player-doc");
  var runner = window.API209WebR;
  if (!CONFIG || !root || !runner) {
    return;
  }

  runner.configure({
    prewarmPackages: CONFIG.prewarmPackages || [],
    dataFiles: CONFIG.dataFiles || []
  });

  var storageOk = (function () {
    try {
      localStorage.setItem("api209-storage-test", "1");
      localStorage.removeItem("api209-storage-test");
      return true;
    } catch (err) {
      return false;
    }
  })();

  var state = { name: "", chunks: {}, answers: {} };
  if (storageOk) {
    try {
      var saved = JSON.parse(localStorage.getItem(CONFIG.storageKey) || "{}");
      if (saved && typeof saved === "object") {
        state.name = saved.name || "";
        state.chunks = saved.chunks || {};
        state.answers = saved.answers || {};
      }
    } catch (err) { /* corrupted save: start fresh */ }
  }

  var dirtySinceExport = false;
  var exportedThisSession = false;

  var saveTimer = null;
  function scheduleSave() {
    dirtySinceExport = true;
    clearTimeout(saveTimer);
    saveTimer = setTimeout(function () {
      var ok = false;
      if (storageOk) {
        try {
          state.savedAt = new Date().toISOString();
          localStorage.setItem(CONFIG.storageKey, JSON.stringify(state));
          ok = true;
        } catch (err) {
          ok = false; /* most likely QuotaExceededError */
        }
      }
      var el = document.getElementById("autosave-status");
      if (el) {
        el.textContent = !storageOk
          ? "⚠ This browser view cannot save between visits — export your .Rmd before closing."
          : ok
            ? "Saved in this browser ✓"
            : "⚠ Could not save — browser storage is full. Download your .Rmd NOW to keep this work.";
      }
    }, 400);
  }

  /* ---------- engine status (honest state machine display) ---------- */

  var STALL_MS = 45000;
  var RETRY_MS = 120000;

  function renderEngine(snap) {
    var el = document.getElementById("engine-status");
    if (!el) { return; }
    var html;
    if (snap.name === "idle") {
      html = "R loads when you start working (~15 MB, one time).";
    } else if (snap.name === "ready") {
      html = "R is ready ✓";
    } else if (snap.name === "failed") {
      html = "⚠ Could not load R in this browser. Your typing is safe and you " +
        "can export anytime. <button type=\"button\" class=\"unlock-toggle\" " +
        "id=\"engine-retry\">Retry setup</button> or use " +
        "<a href=\"getting-started.html\">Posit Cloud</a>.";
    } else { /* starting */
      html = "Setting up R in the background — keep reading and typing. " +
        "<em>(" + snap.detail + ")</em>";
      if (snap.elapsed > STALL_MS) {
        html += "<br>Taking longer than usual — slow connection? Still working…";
      }
      if (snap.elapsed > RETRY_MS) {
        html += " <button type=\"button\" class=\"unlock-toggle\" " +
          "id=\"engine-retry\">Retry setup</button> or use " +
          "<a href=\"getting-started.html\">Posit Cloud</a>.";
      }
    }
    el.innerHTML = html;
  }

  runner.onState(renderEngine);

  document.addEventListener("click", function (event) {
    if (event.target && event.target.id === "engine-retry") {
      runner.retry();
    }
  });

  /* Pre-warm on the first real interaction (typing anywhere in the
     assignment or the name field) — never on page load, to spare metered
     connections. By the first Run, setup is usually already done. */
  var prewarmed = false;
  function armPrewarm() {
    if (prewarmed) { return; }
    prewarmed = true;
    runner.ensure().catch(function () { /* surfaced via engine state */ });
  }

  /* ---------- parse + render ---------- */

  var parsed = null;

  function escapeHtml(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  function inline(s) {
    var out = escapeHtml(s);
    out = out.replace(/`([^`]+)`/g, function (m, code) { return "<code>" + code + "</code>"; });
    out = out.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
    out = out.replace(/&lt;(https?:\/\/[^&\s]+)&gt;/g, '<a href="$1" target="_blank" rel="noopener">$1</a>');
    out = out.replace(/\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
    return out;
  }

  function renderProse(items, container) {
    var paragraph = [];
    var list = null;

    function flushParagraph() {
      if (paragraph.length) {
        var p = document.createElement("p");
        p.innerHTML = inline(paragraph.join(" "));
        container.appendChild(p);
        paragraph = [];
      }
    }
    function flushList() {
      if (list) {
        container.appendChild(list.el);
        list = null;
      }
    }

    items.forEach(function (item) {
      if (item.answerIdx !== undefined) {
        flushParagraph();
        flushList();
        container.appendChild(buildAnswerBox(item.answerIdx, item.placeholder));
        return;
      }
      var line = item.text;
      if (/^\s*$/.test(line)) {
        flushParagraph();
        flushList();
        return;
      }
      var h = /^(#{2,4})\s+(.*)$/.exec(line);
      if (h) {
        flushParagraph();
        flushList();
        var el = document.createElement("h" + h[1].length);
        el.innerHTML = inline(h[2]);
        container.appendChild(el);
        return;
      }
      var img = /^!\[\]\((\S+)\)$/.exec(line);
      if (img) {
        flushParagraph();
        flushList();
        var im = document.createElement("img");
        im.src = img[1];
        im.alt = "";
        im.className = "player-img";
        container.appendChild(im);
        return;
      }
      var li = /^(-|\d+\.)\s+(.*)$/.exec(line);
      if (li) {
        flushParagraph();
        var kind = li[1] === "-" ? "UL" : "OL";
        if (!list || list.kind !== kind) {
          flushList();
          list = { kind: kind, el: document.createElement(kind.toLowerCase()) };
        }
        var liEl = document.createElement("li");
        liEl.innerHTML = inline(li[2]);
        list.el.appendChild(liEl);
        return;
      }
      if (list) {
        var last = list.el.lastElementChild;
        if (last) {
          last.innerHTML += " " + inline(line.trim());
          return;
        }
      }
      paragraph.push(line.trim());
    });
    flushParagraph();
    flushList();
  }

  /* ---------- widgets ---------- */

  function autoGrow(area) {
    area.style.height = "auto";
    area.style.height = Math.max(area.scrollHeight + 2, 44) + "px";
  }

  function buildAnswerBox(idx, placeholder) {
    var wrap = document.createElement("div");
    wrap.className = "answer-box";
    var area = document.createElement("textarea");
    area.placeholder = placeholder.replace(/^\[|\]$/g, "");
    area.value = state.answers[idx] || "";
    area.rows = 2;
    area.addEventListener("input", function () {
      state.answers[idx] = area.value;
      autoGrow(area);
      scheduleSave();
    });
    wrap.appendChild(area);
    requestAnimationFrame(function () { autoGrow(area); });
    return wrap;
  }

  function buildChunk(block) {
    var card = document.createElement("div");
    card.className = "player-chunk";
    card.dataset.editIdx = block.editIdx;

    var bar = document.createElement("div");
    bar.className = "player-chunk-bar";
    var tag = document.createElement("span");
    tag.textContent = "R code — edit and run";
    var reset = document.createElement("button");
    reset.type = "button";
    reset.className = "chunk-reset";
    reset.textContent = "Reset code";
    reset.title = "Restore the original code for this chunk";
    /* Disabled until the student edits: an untouched pre-written chunk has
       nothing to reset, so a live button there only looks broken. */
    reset.disabled = state.chunks[block.editIdx] === undefined;
    bar.appendChild(tag);
    bar.appendChild(reset);
    card.appendChild(bar);

    var area = document.createElement("textarea");
    area.className = "code-editor";
    area.spellcheck = false;
    area.value = state.chunks[block.editIdx] !== undefined
      ? state.chunks[block.editIdx]
      : block.body;
    area.addEventListener("input", function () {
      state.chunks[block.editIdx] = area.value;
      reset.disabled = false;
      autoGrow(area);
      scheduleSave();
    });
    card.appendChild(area);
    requestAnimationFrame(function () { autoGrow(area); });

    var runBar = document.createElement("div");
    runBar.className = "run-bar";
    var runBtn = document.createElement("button");
    runBtn.type = "button";
    runBtn.className = "run-code-button";
    runBtn.textContent = "▶ Run";
    runBtn.title = "Run this chunk (⌘/Ctrl + Enter)";
    var status = document.createElement("span");
    status.className = "run-status";
    status.setAttribute("aria-live", "polite");
    var clearOut = document.createElement("button");
    clearOut.type = "button";
    clearOut.className = "chunk-clear-output";
    clearOut.textContent = "Clear output";
    clearOut.title = "Clear this chunk's output so you can run it fresh";
    clearOut.hidden = true;
    runBar.appendChild(runBtn);
    runBar.appendChild(clearOut);
    runBar.appendChild(status);
    card.appendChild(runBar);

    var plots = document.createElement("div");
    plots.className = "run-plots";
    card.appendChild(plots);

    var output = document.createElement("pre");
    output.className = "run-output";
    output.hidden = true;
    card.appendChild(output);

    reset.addEventListener("click", function () {
      area.value = block.body;
      delete state.chunks[block.editIdx];
      reset.disabled = true;
      autoGrow(area);
      scheduleSave();
    });
    clearOut.addEventListener("click", function () {
      output.hidden = true;
      output.textContent = "";
      plots.innerHTML = "";
      clearOut.hidden = true;
    });
    runBtn.addEventListener("click", function () {
      runChunk(area.value, runBtn, status, output, plots, clearOut);
    });
    /* Cmd/Ctrl + Enter (with or without Shift, matching RStudio) runs the
       chunk, so the keyboard hint shown in the assignment text is true in
       the browser too. A plain Enter still inserts a newline. */
    area.addEventListener("keydown", function (event) {
      if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
        event.preventDefault();
        if (!runBtn.disabled) {
          runChunk(area.value, runBtn, status, output, plots, clearOut);
        }
      }
    });
    return card;
  }

  function buildStatic(block) {
    var pre = document.createElement("pre");
    pre.className = "folder-tree";
    var code = document.createElement("code");
    code.textContent = block.body;
    pre.appendChild(code);
    return pre;
  }

  async function runChunk(code, button, status, output, plots, clearOut) {
    button.disabled = true;
    status.textContent = runner.getState().name === "ready"
      ? "Running…"
      : "Waiting for R setup (see the panel)…";
    try {
      var result = await runner.runCode(code, { graphics: true });
      status.textContent = "Running…";
      plots.innerHTML = "";
      (result.images || []).forEach(function (img) {
        var canvas = document.createElement("canvas");
        canvas.width = img.width;
        canvas.height = img.height;
        canvas.className = "run-plot";
        canvas.getContext("2d").drawImage(img, 0, 0);
        plots.appendChild(canvas);
      });
      output.textContent = result.text ||
        (plots.children.length
          ? ""
          : "(no printed output — objects may still have been created)");
      output.hidden = !output.textContent;
      status.textContent = "";
    } catch (err) {
      var msg = String(err && err.message ? err.message : err);
      if (runner.getState().name === "failed") {
        status.textContent = "R could not load — your code is kept; see the panel.";
      } else {
        output.textContent = /^Error/i.test(msg) ? msg : "Error: " + msg;
        output.hidden = false;
        status.textContent = "";
      }
    } finally {
      button.disabled = false;
      if (clearOut) {
        /* Offer "Clear output" whenever there is something to clear. */
        clearOut.hidden = output.hidden && !plots.children.length;
      }
    }
  }

  async function resetSession() {
    try {
      var had = await runner.resetSession();
      document.querySelectorAll("#player-doc .run-output").forEach(function (o) {
        o.hidden = true;
        o.textContent = "";
      });
      document.querySelectorAll("#player-doc .run-plots").forEach(function (p) {
        p.innerHTML = "";
      });
      document.querySelectorAll("#player-doc .chunk-clear-output").forEach(function (b) {
        b.hidden = true;
      });
      var el = document.getElementById("engine-status");
      if (el) {
        el.textContent = had
          ? "R session reset — objects cleared ✓"
          : "R has not started yet — nothing to reset.";
      }
    } catch (err) {
      var el2 = document.getElementById("engine-status");
      if (el2) { el2.textContent = "Could not reset the session."; }
    }
  }

  /* ---------- export ---------- */

  function buildRmd() {
    return buildRmdFrom(parsed, state, state.name);
  }

  /* Every download reminds students to run everything and fix
     errors/warnings first; escalates when a visible output shows one. */
  function confirmDownload() {
    var outputs = Array.prototype.slice.call(
      document.querySelectorAll("#player-doc .run-output:not([hidden])")
    );
    var hasIssue = outputs.some(function (o) {
      return /(^|\n)\s*(Error|Warning)/i.test(o.textContent);
    });
    var msg = hasIssue
      ? "⚠ At least one chunk's output currently shows an error or warning.\n\n" +
        "The exported file contains your code exactly as written. Fix the " +
        "problem and re-run the chunk if you can.\n\nDownload anyway?"
      : "Quick check before you download:\n\n" +
        "• Have you run your chunks from top to bottom?\n" +
        "• Is the output free of errors and warnings?\n\n" +
        "The exported file contains your code exactly as written.\n\nDownload now?";
    return window.confirm(msg);
  }

  function renderExportStatus() {
    var el = document.getElementById("export-status");
    if (el) {
      el.textContent = exportedThisSession
        ? "Last downloaded: " + new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })
        : "Not downloaded yet in this session.";
    }
  }

  function downloadRmd() {
    if (!confirmDownload()) { return; }
    var clean = safeFilePart(state.name || "");
    var filename = clean
      ? CONFIG.exportBase + " - " + clean + ".Rmd"
      : CONFIG.exportBase + ".Rmd";
    var blob = new Blob([buildRmd()], { type: "text/plain;charset=utf-8" });
    var a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(function () { URL.revokeObjectURL(a.href); }, 5000);
    dirtySinceExport = false;
    exportedThisSession = true;
    renderExportStatus();
  }

  window.api209Player = {
    buildRmd: function () { return buildRmd(); },
    download: downloadRmd,
    resetSession: resetSession
  };

  /* Warn on close only when there is real, never-exported work this
     session — constant nagging trains students to ignore it. */
  window.addEventListener("beforeunload", function (event) {
    var hasWork = Object.keys(state.chunks).length ||
      Object.keys(state.answers).some(function (k) {
        return String(state.answers[k]).trim();
      });
    if (dirtySinceExport && hasWork && !exportedThisSession) {
      event.preventDefault();
      event.returnValue = "";
    }
  });

  /* ---------- boot ---------- */

  function wirePanel() {
    var nameInput = document.getElementById("student-name");
    if (nameInput) {
      nameInput.value = state.name;
      nameInput.addEventListener("input", function () {
        state.name = nameInput.value;
        armPrewarm();
        scheduleSave();
      });
    }
    document.querySelectorAll("[data-player-download]").forEach(function (btn) {
      btn.addEventListener("click", downloadRmd);
    });
    var resetBtn = document.getElementById("reset-session");
    if (resetBtn) { resetBtn.addEventListener("click", resetSession); }
    var clearBtn = document.getElementById("clear-work");
    if (clearBtn) {
      clearBtn.addEventListener("click", function () {
        var ok = window.confirm("Delete all saved code and answers for this part in this browser? This cannot be undone.");
        if (!ok) { return; }
        try { localStorage.removeItem(CONFIG.storageKey); } catch (err) { /* ignore */ }
        location.reload();
      });
    }
    root.addEventListener("input", armPrewarm);
    renderExportStatus();
    if (!storageOk) {
      var note = document.getElementById("autosave-status");
      if (note) {
        note.textContent = "⚠ This browser view cannot save your work between visits — export your .Rmd before closing.";
      }
    }
  }

  /* no-cache = revalidate with the server; if a stale cache still hands us
     a broken copy (e.g. an old CDN/browser entry), retry once with a
     cache-busting URL before declaring failure — self-healing for the
     stale-source class of errors. */
  function fetchSource(bustCache) {
    var url = CONFIG.source;
    if (bustCache) {
      url += (url.indexOf("?") === -1 ? "?" : "&") + "fresh=" + Date.now();
    }
    return fetch(url, { cache: bustCache ? "reload" : "no-cache" })
      .then(function (res) {
        if (!res.ok) { throw new Error("HTTP " + res.status); }
        return res.text();
      })
      .then(function (text) {
        var p = parseRmd(text);
        var problems = validateParse(p, text);
        if (problems.length) {
          if (!bustCache) { return fetchSource(true); }
          var err = new Error(problems.join("; "));
          err.isParse = true;
          throw err;
        }
        return p;
      });
  }

  fetchSource(false)
    .then(function (result) {
      parsed = result;
      parsed.blocks.forEach(function (block) {
        if (block.type === "chunk") {
          if (!block.hidden) { root.appendChild(buildChunk(block)); }
        } else if (block.type === "static") {
          root.appendChild(buildStatic(block));
        } else {
          renderProse(block.items, root);
        }
      });
      wirePanel();
      var loading = document.getElementById("player-loading");
      if (loading) { loading.remove(); }
    })
    .catch(function (err) {
      var loading = document.getElementById("player-loading");
      if (loading) {
        loading.innerHTML = err && err.isParse
          ? "⚠ This assignment didn't load correctly (" +
            escapeHtml(err.message) + "). Reload the page; if it persists, " +
            "use <a href=\"getting-started.html\">Posit Cloud</a> and tell " +
            "the teaching team."
          : "Could not load the assignment in this view. " +
            "Open it in <a href=\"getting-started.html\">Posit Cloud</a> instead.";
      }
    });
})();
