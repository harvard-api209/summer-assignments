/* Shared interactions for the API-209 summer guide.
   Everything degrades gracefully: with JavaScript off, the pages stay
   fully readable and the static "Getting Started" next-step card applies. */
(function () {
  "use strict";

  var COURSE_KEY = "api209-course-progress-v1";
  var WARMUP_PROGRESS_KEY = "api209-warmup-progress-v2";
  var WARMUP_ANSWER_KEY = "api209-warmup-answers-v2";
  /* The share link creates a NEW temporary copy each visit, so it is only
     for the first visit. Returning students must reopen their saved copy
     from their own workspace, or they will work in (and lose) a copy.
     These constants are the single source of truth for platform links:
     a future platform change starts by editing them (static hrefs in the
     HTML are the no-JS fallback and should be swept afterwards). */
  var WORKSPACE_URL = "https://posit.cloud";
  var PROJECT_SHARE_URL = "https://posit.cloud/content/8155534";
  var REPO_ZIP_URL =
    "https://github.com/harvard-api209/summer-assignments/archive/refs/heads/2026-refresh.zip";

  /* The five assignments are the only counted thing on the site. Setup
     and the warm-up are optional (browser-first, see plans/007) and are
     never steps or denominators. */
  var PART_IDS = ["part-1", "part-2", "part-3", "part-4", "part-5"];
  /* Answer-box totals per part, used for "in progress" badges. Recount
     with: grep -c "^\[Write your" docs/web-assignments/part-N.Rmd */
  var PART_ANSWER_TOTALS = { 1: 7, 2: 5, 3: 4, 4: 4, 5: 8 };

  /* Trackable steps, in course order. The submit step is a destination,
     not a checkbox: it becomes the "next step" once everything is done. */
  var STEPS = [
    {
      id: "part-1",
      title: "Part 1: Programming Fundamentals",
      blurb:
        "Start right here in your browser — no account, no setup. Your " +
        "work saves automatically as you type.",
      href: "part1.html",
      cta: "Start Part 1"
    },
    {
      id: "part-2",
      title: "Part 2: Filtering Country-Year Data",
      blurb:
        "On the Part 2 page, read the course CSV, use the data dictionary, " +
        "filter and sort rows, and interpret missingness.",
      href: "part2.html",
      cta: "Open Part 2"
    },
    {
      id: "part-3",
      title: "Part 3: Data Visualization",
      blurb:
        "On the Part 3 page, build descriptive plots with ggplot(), label " +
        "figures, and explain what each graph shows.",
      href: "part3.html",
      cta: "Open Part 3"
    },
    {
      id: "part-4",
      title: "Part 4: Data Manipulation II",
      blurb:
        "On the Part 4 page, use group_by() and summarise(), compare means " +
        "and medians, and treat missing values as a data-quality issue.",
      href: "part4.html",
      cta: "Open Part 4"
    },
    {
      id: "part-5",
      title: "Part 5: Mini Diagnostic Memo",
      blurb:
        "On the Part 5 page, choose one country and coherent peers, produce " +
        "the required outputs, and write the short non-causal memo.",
      href: "part5.html",
      cta: "Open Part 5"
    }
  ];

  var SUBMIT_STEP = {
    title: "Submit on Canvas",
    blurb:
      "All five parts are marked complete. Make sure you downloaded each " +
      "completed .Rmd, then follow the submission steps.",
    href: "submission.html",
    cta: "Read the submission steps"
  };

  /* Sandboxed contexts (file previews, some embedded webviews) block
     localStorage. Detect it once so the UI can say so instead of silently
     losing progress between visits. */
  var storageOk = (function () {
    try {
      localStorage.setItem("api209-storage-test", "1");
      localStorage.removeItem("api209-storage-test");
      return true;
    } catch (err) {
      return false;
    }
  })();

  function readJSON(key) {
    try {
      return JSON.parse(localStorage.getItem(key) || "{}") || {};
    } catch (err) {
      return {};
    }
  }

  function loadProgress() {
    var progress = readJSON(COURSE_KEY);
    /* The warm-up page keeps its own tracker; completing it there
       counts here automatically. */
    if (readJSON(WARMUP_PROGRESS_KEY).finish) {
      progress.warmup = true;
    }
    return progress;
  }

  function saveProgress(progress) {
    try {
      localStorage.setItem(COURSE_KEY, JSON.stringify(progress));
    } catch (err) {
      /* Private browsing or storage disabled: tracking is best-effort. */
    }
  }

  var progress = loadProgress();

  function doneCount() {
    return STEPS.filter(function (step) {
      return progress[step.id];
    }).length;
  }

  function partsDone() {
    return PART_IDS.filter(function (id) {
      return progress[id];
    }).length;
  }

  /* Reads the in-browser player's autosave (written by
     assignment-player.js) so the roadmap can show live "in progress"
     state. Read-only: the schema belongs to the player. */
  function partWork(n) {
    try {
      var raw = localStorage.getItem("api209-part" + n + "-work-v1");
      if (!raw) {
        return null;
      }
      var w = JSON.parse(raw) || {};
      var answers = 0;
      Object.keys(w.answers || {}).forEach(function (k) {
        if (String(w.answers[k]).trim()) {
          answers += 1;
        }
      });
      var chunks = Object.keys(w.chunks || {}).length;
      if (!answers && !chunks) {
        return null;
      }
      return { answers: answers, chunks: chunks, total: PART_ANSWER_TOTALS[n] || 0 };
    } catch (err) {
      return null;
    }
  }

  function nextStep() {
    /* Prefer the lowest-numbered incomplete part the student has actually
       started; otherwise the first incomplete part. */
    var firstIncomplete = null;
    for (var i = 0; i < STEPS.length; i += 1) {
      if (!progress[STEPS[i].id]) {
        if (!firstIncomplete) {
          firstIncomplete = STEPS[i];
        }
        if (partWork(i + 1)) {
          return STEPS[i];
        }
      }
    }
    return firstIncomplete || SUBMIT_STEP;
  }

  /* ---------- Current page in the navigation ---------- */

  function markCurrentNavLink() {
    var page = location.pathname.split("/").pop() || "index.html";
    document.querySelectorAll(".nav-links a").forEach(function (link) {
      var raw = link.getAttribute("href") || "";
      if (raw.charAt(0) === "#") {
        return; /* same-page anchors are not a separate page */
      }
      if (raw.split("#")[0] === page && !link.closest(".nav-menu")) {
        link.setAttribute("aria-current", "page");
      }
    });
    /* The Assignments dropdown represents the hub and the five part pages. */
    var dropdown = document.querySelector(".nav-dropdown");
    if (dropdown) {
      var owns = Array.prototype.some.call(
        dropdown.querySelectorAll(".nav-menu a"),
        function (a) { return (a.getAttribute("href") || "").split("#")[0] === page; }
      );
      if (owns) {
        dropdown.querySelector("summary").setAttribute("aria-current", "page");
      }
      document.addEventListener("click", function (event) {
        if (dropdown.open && !dropdown.contains(event.target)) {
          dropdown.open = false;
        }
      });
    }
  }

  /* ---------- Progress checkboxes (any page) ---------- */

  function syncCheckboxes() {
    document.querySelectorAll("input[data-course-step]").forEach(function (box) {
      box.checked = Boolean(progress[box.dataset.courseStep]);
    });
  }

  function bindCheckboxes() {
    document.addEventListener("change", function (event) {
      var box = event.target;
      if (!box.matches || !box.matches("input[data-course-step]")) {
        return;
      }
      progress[box.dataset.courseStep] = box.checked;
      saveProgress(progress);
      renderIndex();
    });
  }

  /* ---------- Homepage dashboard ---------- */

  function injectAssignmentCheckboxes() {
    ["part-1", "part-2", "part-3", "part-4", "part-5"].forEach(function (id) {
      var article = document.getElementById(id);
      if (!article || article.querySelector("input[data-course-step]")) {
        return;
      }
      var label = document.createElement("label");
      label.className = "done-check course-check";
      var input = document.createElement("input");
      input.type = "checkbox";
      input.dataset.courseStep = id;
      label.appendChild(input);
      label.appendChild(document.createTextNode(" Mark this part complete"));
      article.appendChild(label);
    });
  }

  function injectProgressSummary() {
    var panel = document.querySelector(".dashboard-panel");
    if (!panel || panel.querySelector(".course-progress")) {
      return;
    }
    var wrap = document.createElement("div");
    wrap.className = "course-progress";
    wrap.innerHTML =
      '<div class="progress-row">' +
      '<span class="progress-label">Parts complete</span>' +
      '<strong><span data-progress-count>0</span>/' + PART_IDS.length + "</strong>" +
      "</div>" +
      '<div class="progress-track" aria-hidden="true"><span data-progress-bar></span></div>' +
      '<p class="progress-live" data-progress-live hidden></p>' +
      '<p class="progress-note">' +
      (storageOk
        ? "Progress is saved in this browser only. Mark parts complete in the roadmap below."
        : "This browser view cannot save progress between visits. Open the site in a regular browser tab to keep it.") +
      "</p>" +
      '<button type="button" class="progress-reset" data-progress-reset hidden>Reset my progress</button>';
    panel.appendChild(wrap);

    wrap.querySelector("[data-progress-reset]").addEventListener("click", function () {
      var ok = window.confirm(
        "Reset your saved progress on this site? This also resets the warm-up activities."
      );
      if (!ok) {
        return;
      }
      try {
        localStorage.removeItem(COURSE_KEY);
        localStorage.removeItem(WARMUP_PROGRESS_KEY);
        localStorage.removeItem(WARMUP_ANSWER_KEY);
      } catch (err) {
        /* ignore */
      }
      progress = {};
      syncCheckboxes();
      renderIndex();
    });
  }

  function renderNextCard() {
    var card = document.querySelector(".next-card");
    if (!card) {
      return;
    }
    var step = nextStep();
    var done = doneCount();
    var kicker = card.querySelector("span");
    var heading = card.querySelector("h2");
    var copy = card.querySelector("p");
    var button = card.querySelector("a.button");

    var started = step !== SUBMIT_STEP && partWork(PART_IDS.indexOf(step.id) + 1);
    if (kicker) {
      kicker.textContent = step === SUBMIT_STEP
        ? "Final step"
        : (started ? "Continue · " : "Next up · ") +
          partsDone() + " of " + PART_IDS.length + " parts done";
    }
    if (heading) {
      heading.textContent = step.title;
    }
    if (copy) {
      copy.textContent = step.blurb;
    }
    if (button) {
      button.textContent = started
        ? step.cta.replace(/^(Start|Open)/, "Continue")
        : step.cta;
      button.href = step.href;
      if (step.external) {
        button.target = "_blank";
        button.rel = "noopener";
      } else {
        button.removeAttribute("target");
        button.removeAttribute("rel");
      }
    }
  }

  function renderAssignments() {
    PART_IDS.forEach(function (id, index) {
      var article = document.getElementById(id);
      if (!article) {
        return;
      }
      var done = Boolean(progress[id]);
      article.classList.toggle("is-done", done);
      var work = done ? null : partWork(index + 1);
      var status = article.querySelector(".assignment-status");
      if (work) {
        if (!status) {
          status = document.createElement("p");
          status.className = "assignment-status";
          var h3 = article.querySelector("h3");
          h3.parentNode.insertBefore(status, h3.nextSibling);
        }
        status.textContent = work.total
          ? "In progress · " + work.answers + " of " + work.total + " answers written"
          : "In progress";
      } else if (status) {
        status.remove();
      }
    });
  }

  function renderProgressSummary() {
    var count = document.querySelector("[data-progress-count]");
    var bar = document.querySelector("[data-progress-bar]");
    var reset = document.querySelector("[data-progress-reset]");
    if (!count || !bar) {
      return;
    }
    var parts = partsDone();
    count.textContent = parts;
    bar.style.width = (parts / PART_IDS.length) * 100 + "%";
    var liveEl = document.querySelector("[data-progress-live]");
    if (liveEl) {
      var inProgress = [];
      PART_IDS.forEach(function (id, index) {
        if (!progress[id] && partWork(index + 1)) {
          inProgress.push("Part " + (index + 1));
        }
      });
      liveEl.hidden = !inProgress.length;
      liveEl.textContent = inProgress.length
        ? "In progress: " + inProgress.join(", ")
        : "";
    }
    if (reset) {
      reset.hidden = doneCount() === 0;
    }
  }

  function renderIndex() {
    renderNextCard();
    renderAssignments();
    renderProgressSummary();
  }

  function initIndex() {
    if (!document.querySelector(".course-dashboard")) {
      return;
    }
    injectAssignmentCheckboxes();
    injectProgressSummary();
    syncCheckboxes();
    renderIndex();
  }

  /* ---------- FAQ filter ---------- */

  function initFaqFilter() {
    var input = document.getElementById("faq-filter");
    if (!input) {
      return;
    }
    var search = input.closest(".faq-search");
    if (search) {
      search.hidden = false;
    }
    var articles = Array.prototype.slice.call(
      document.querySelectorAll(".faq-list article")
    );
    var sections = Array.prototype.slice.call(
      document.querySelectorAll(".resource-section")
    ).filter(function (section) {
      return section.querySelector(".faq-list");
    });
    var count = document.querySelector(".faq-count");

    function apply() {
      var query = input.value.trim().toLowerCase();
      var shown = 0;
      articles.forEach(function (article) {
        var match = !query ||
          article.textContent.toLowerCase().indexOf(query) !== -1;
        article.hidden = !match;
        if (match) {
          shown += 1;
        }
      });
      sections.forEach(function (section) {
        var visible = section.querySelectorAll(".faq-list article:not([hidden])");
        section.hidden = Boolean(query) && visible.length === 0;
      });
      if (count) {
        count.textContent = query
          ? shown === 0
            ? "No questions match. Try a shorter word, or contact the teaching team below."
            : "Showing " + shown + " of " + articles.length + " questions"
          : "";
      }
    }

    input.addEventListener("input", apply);
  }

  /* ---------- Platform links ---------- */

  /* Single source of truth for platform links. Static hrefs in the HTML
     are the no-JS fallback and MUST stay correct; this rewrite exists so
     a future platform change is: update the constants at the top of this
     file, then sweep the static hrefs at leisure. */
  function applyPlatformLinks() {
    var urls = {
      workspace: WORKSPACE_URL,
      project: PROJECT_SHARE_URL,
      zip: REPO_ZIP_URL
    };
    document.querySelectorAll("[data-platform-link]").forEach(function (a) {
      var url = urls[a.dataset.platformLink];
      if (url) {
        a.href = url;
      }
    });
  }

  /* ---------- FAQ anchor links ---------- */

  function slugify(text) {
    return text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, "-")
      .replace(/^-+|-+$/g, "")
      .slice(0, 60);
  }

  function initFaqAnchors() {
    /* Only on the FAQ page: every question gets a stable id so the
       teaching team can email students a link to one specific answer. */
    if (!document.getElementById("faq-filter")) {
      return;
    }
    document.querySelectorAll(".faq-list article").forEach(function (article) {
      var heading = article.querySelector("h3");
      if (!heading) {
        return;
      }
      if (!article.id) {
        article.id = "q-" + slugify(heading.textContent);
      }
      if (heading.querySelector(".anchor-link")) {
        return;
      }
      var link = document.createElement("a");
      link.className = "anchor-link";
      link.href = "#" + article.id;
      link.textContent = "#";
      link.setAttribute("aria-label", "Link to this question");
      heading.appendChild(document.createTextNode(" "));
      heading.appendChild(link);
    });
    /* The ids above are created after the browser's own fragment
       navigation, so finish the jump manually. */
    if (location.hash) {
      var target = document.getElementById(location.hash.slice(1));
      if (target && target.closest(".faq-list")) {
        target.scrollIntoView();
      }
    }
  }

  /* ---------- Back to top ---------- */

  function initBackToTop() {
    var button = document.createElement("button");
    button.type = "button";
    button.className = "back-to-top";
    button.setAttribute("aria-label", "Back to top");
    button.textContent = "↑";
    document.body.appendChild(button);

    function onScroll() {
      button.classList.toggle("is-visible", window.scrollY > 600);
    }

    window.addEventListener("scroll", onScroll, { passive: true });
    onScroll();
    button.addEventListener("click", function () {
      var reduced = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
      window.scrollTo({ top: 0, behavior: reduced ? "auto" : "smooth" });
    });
  }

  /* Part pages open in their own tabs and write progress/work to
     localStorage there; an already-open homepage must hear about it.
     `storage` fires for changes made in OTHER tabs; visibilitychange
     covers coming back to a tab the browser kept alive. */
  function refreshFromStorage() {
    progress = loadProgress();
    syncCheckboxes();
    renderIndex();
  }

  window.addEventListener("storage", function (event) {
    if (!event.key || event.key.indexOf("api209-") === 0) {
      refreshFromStorage();
    }
  });

  document.addEventListener("visibilitychange", function () {
    if (!document.hidden) {
      refreshFromStorage();
    }
  });

  markCurrentNavLink();
  applyPlatformLinks();
  bindCheckboxes();
  syncCheckboxes();
  initIndex();
  initFaqFilter();
  initFaqAnchors();
  initBackToTop();
})();
