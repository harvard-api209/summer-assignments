# Plan 003: Spike — in-browser runnable R (webR) for the warm-up page

> **Executor instructions**: This is a SPIKE, not a feature build. The
> deliverable is a working prototype on a branch plus a findings memo —
> NOT a merged feature. Follow the steps, honor the STOP conditions, and
> write the memo even if the recommendation is "don't do it." When done,
> update the status row in `plans/README.md`.
>
> **Drift check (run first)**: `git diff --stat e2dfc1c..HEAD -- docs/interactive-hour.html docs/site.css`
> On mismatch with the "Current state" excerpts, STOP and report.

## Status

- **Priority**: P3
- **Effort**: M (timebox: one day; stop at the timebox even if incomplete)
- **Risk**: MED for the eventual feature (payload size, browser support);
  LOW for the spike itself (isolated branch, no production change)
- **Depends on**: none (but coordinate with 001/006, which edit the same file)
- **Category**: direction
- **Planned at**: commit `e2dfc1c`, 2026-06-11

## Why this matters

The course professor shared a reference site,
`https://teddysvoronos.com/r-primer/unit-1.html` (an API-201 summer R
primer), as a model to learn from. Its standout feature: every code
example has a **"Run Code" button that executes R in the browser via
webR** (R compiled to WebAssembly) — students run and *edit* real R with
zero accounts and zero installation. Our warm-up page
(`docs/interactive-hour.html`) already copies that site's pedagogy —
read the code, predict the output, check — but our code cards are static
HTML; the student cannot run anything until they enter Posit Cloud. Adding
execution would close that gap and also reduce the site's dependence on
Posit Cloud for the practice portion (a concern a faculty reviewer raised
explicitly). The open questions are cost ones: payload size, load time,
GitHub Pages compatibility, and code complexity. This spike answers them.

## Current state

- `docs/interactive-hour.html` — the warm-up page. Code examples are
  static "code cards", e.g. Warm-up 1 (around line 107):

  ```html
  <article class="code-card">
    <pre><code><span class="line"><span class="line-number">1</span><span>2 + 2</span></span></code></pre>
  </article>
  ```

  The page has one large inline `<script>` at the bottom handling
  progress, locking, and quiz feedback. Progress state lives in
  localStorage (`api209-warmup-progress-v2`, `api209-warmup-answers-v2`).

- Hosting: GitHub Pages, static, from branch `2026-refresh` `/docs`.
  **You cannot set custom HTTP headers on GitHub Pages** — no
  Cross-Origin-Opener-Policy / Cross-Origin-Embedder-Policy. webR's
  fastest channel (SharedArrayBuffer) requires those headers; webR also
  supports a `PostMessage` channel that works WITHOUT cross-origin
  isolation, at reduced performance. The spike must use the PostMessage
  channel (or the `ServiceWorker` channel) and measure whether it is fast
  enough.

- webR documentation: `https://docs.r-wasm.org/webr/latest/` — read
  "Getting started (Web)" and "Communication channels" before coding.
  webR loads from CDN: `https://webr.r-wasm.org/latest/webr.mjs`.

- Design conventions: white/crimson GitHub-ish aesthetic in
  `docs/site.css`; dark code cards (`.code-card`, `#0f1720` background)
  with a mac-style title bar (`.code-card::before`).

## Commands you will need

| Purpose | Command | Expected on success |
|---------|---------|---------------------|
| Serve locally | `python3 -m http.server 8210 --directory docs` | site at :8210 |
| Payload measurement | browser DevTools → Network tab, "Disable cache" | total transferred & load time recorded |

## Scope

**In scope** (on the spike branch only):
- `docs/interactive-hour.html` — add ONE runnable chunk (Warm-up 1's
  `2 + 2` card) behind a "Run this code" button.
- `docs/site.css` — minimal styles for the run button/output area.
- `plans/003-findings.md` — the memo (created by this spike).

**Out of scope**:
- Converting all warm-up sections (that is the follow-up feature, only if
  the memo recommends it).
- Any change to the assignments or to Posit Cloud links.
- Bundlers, package.json, or build tooling — load webR from its CDN as an
  ES module; this repo stays build-free.

## Git workflow

- Branch off `2026-refresh`: `advisor/003-webr-spike`
- This branch is throwaway evidence. Do NOT push or merge.

## Steps

### Step 1: Lazy-load webR behind an explicit click

Add to the Warm-up 1 code card a "Run this code" button and an output
area. webR must NOT load on page load — only on first click (protects
students on slow connections who never click). Sketch:

```html
<button type="button" class="button secondary run-code-button"
        data-code="2 + 2">Run this code</button>
<pre class="run-output" hidden aria-live="polite"></pre>
```

```js
let webRPromise = null;
function getWebR() {
  if (!webRPromise) {
    webRPromise = import("https://webr.r-wasm.org/latest/webr.mjs")
      .then(({ WebR }) => {
        const webR = new WebR({ channelType: 3 /* PostMessage */ });
        return webR.init().then(() => webR);
      });
  }
  return webRPromise;
}
```

On click: show "Loading R (one-time, ~10–20 MB)…", await `getWebR()`,
run the chunk via `webR.evalRString` / `Shelter` API (follow the current
webR docs — the exact API may have moved since this plan was written),
print the result into `.run-output`.

**Verify**: served locally, clicking "Run this code" eventually prints
`[1] 4`. Reloading and NOT clicking → Network tab shows no webR download.

### Step 2: Measure

Record in the memo, from a clean cache:

1. Total bytes transferred for the first "Run" click (webR core + R wasm).
2. Time from click to `[1] 4` on (a) your machine, (b) DevTools "Slow 4G"
   throttling.
3. Time for a SECOND run (warm) — should be near-instant.
4. Whether the PostMessage channel shows console warnings or failures in
   Chrome and Safari (test both; Safari is common among students).
5. Memory: DevTools task manager — RSS of the tab after init.

### Step 3: Stress the realistic case

Replace the test snippet with the kind of code later warm-ups would need:
`mean(c(120, 150, 180))` works with base R, but the course teaches
tidyverse pipes (`poverty |> filter(country == "Ghana")`). Check: does
`webr::install("dplyr")` (or the `repos` option pointing at the webR
binary repo) work over the PostMessage channel on GitHub-Pages-like
conditions, and how long does installing dplyr take? Record it. If
tidyverse is impractical, note whether base-R-only warm-ups (units 1–3:
arithmetic, objects, functions) would still be worth it.

### Step 4: Write `plans/003-findings.md`

Structure: Numbers (from steps 2–3) / What worked / What broke /
Recommendation — one of:

- **Adopt**: convert warm-ups 1–3 (base R) to runnable chunks; keep 4–7
  static. Include an effort estimate.
- **Adopt-partial / defer**: e.g. only if payload < X MB and Safari works.
- **Reject**: static cards + Posit Cloud remain better; say why in two
  sentences a professor can act on.

## Test plan

The spike IS the test. Done-ness is the memo, not test coverage.

## Done criteria

- [ ] Spike branch contains a working (or demonstrably failing) runnable
      `2 + 2` chunk, lazy-loaded on click
- [ ] `plans/003-findings.md` exists with all measurements from Steps 2–3
      and a single clear recommendation
- [ ] No changes on `2026-refresh` itself; production site untouched
- [ ] `plans/README.md` status row updated

## STOP conditions

- The timebox (one working day) expires — write the memo with whatever
  was learned; an incomplete spike with honest findings is a success.
- webR's current API differs so much from this plan's sketch that the
  docs require choosing between architectures (e.g. service-worker
  channel needs a `sw.js` at site root) — record the options in the memo
  and stop rather than building both.
- The PostMessage channel fundamentally fails on GitHub Pages-style
  hosting (no SAB, no custom headers) — that alone is a "Reject" or
  "needs different hosting" finding; write it up.

## Maintenance notes

- If adopted: webR pins should use a versioned CDN URL (not `/latest/`)
  before production, and the warm-up's existing quiz-locking logic must
  treat "ran the code" as optional — never gate progress on a 20 MB
  download succeeding.
- If rejected: keep the memo; the question will come back next summer and
  the numbers will save a day.
