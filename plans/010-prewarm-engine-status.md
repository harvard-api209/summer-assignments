# Plan 010: Pre-warm R in the background + an honest engine status machine

> **Executor instructions**: Follow steps + verifications; STOP conditions
> binding; update `plans/README.md` when done.
>
> **CRITICAL — working-tree state**: `docs/assignment-player.js` is
> UNCOMMITTED on `2026-refresh`. Work in the existing working tree. NO
> commits/pushes.
>
> **Drift check (run first)**: open `docs/assignment-player.js` and
> confirm `getWebR()` exists with `webRPromise` caching and
> `stageDataFiles()`; part pages define `window.API209_PART` with
> `dataFiles`. On mismatch with the excerpts below, STOP.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: MED (touches the runner everything depends on; mitigated by
  the test plan and by changing *when* setup starts, not *what* it does)
- **Depends on**: none (coordinate with 011, same file — run 010 first)
- **Category**: direction (grilled decision: option b + explicit
  requirement: the student must always be able to tell whether setup is
  hanging, progressing, or ready)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12

## Why this matters

The worst moment in the browser path is the first Run in Parts 2–5: webR
(~15 MB) plus a tidyverse install (minutes) all happen at the exact moment
the student first tries to act. Students read and type for several minutes
before that first Run — dead time we can use. Decision: start engine
setup silently at the student's **first interaction** with the page
(commitment signal — never on mere page load, to spare metered
connections), and replace the single status string with an honest state
machine: the student can always tell *setting up → still working → ready*
apart, and a stall offers retry + the Posit fallback.

## Current state

`docs/assignment-player.js` (all line numbers from the current working
tree — re-grep before editing):

```js
// :349 (inside getWebR)
webRPromise = import("https://webr.r-wasm.org/latest/webr.mjs")
  .then(async function (mod) {
    var webR = new mod.WebR();
    await webR.init();
    await webR.evalRVoid("webr::shim_install()");
    await stageDataFiles(webR);
    setEngineStatus("R is ready ✓");
    return webR;
  })
```

- `setEngineStatus(text)` writes `#engine-status` (a `panel-note` in each
  part page's aside).
- `runChunk()` calls `getWebR()` lazily on first Run; a `heavy` regex
  switches the status text for package-installing chunks.
- Packages themselves install when the student runs the assignment's own
  "Packages and data" chunk (`install.packages("tidyverse")`, which works
  via `webr::shim_install()`); the engine pre-warm must ALSO pre-install
  tidyverse for parts 2–5 or the chunk still costs minutes. Part pages
  config: `window.API209_PART.dataFiles` is non-empty exactly for parts
  2–5 — use that as the "needs tidyverse" signal, or add an explicit
  `prewarmPackages: ["tidyverse"]` to the config blocks in
  `docs/part2.html`…`part5.html` (preferred — explicit beats inferred).

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Serve | `python3 -m http.server 8210 --directory docs` | :8210 |
| JS syntax | `node --check docs/assignment-player.js` | exit 0 |

## Scope

**In scope**: `docs/assignment-player.js`, `docs/part2.html`…`part5.html`
(config addition), `docs/part1.html` (prewarm with no packages), small CSS
for the status states, `?v=`/`assignment-player.js?v=` bumps in the six
player pages.

**Out of scope**: the warm-up page's separate runner (plan 011
consolidates); pinning the CDN version (011); homepage.

## Steps

### Step 1: Engine state machine

Replace the free-text `setEngineStatus` discipline with explicit states.
Add at module level:

```js
var engine = { state: "idle", detail: "", since: 0 };
function setEngine(state, detail) {
  engine.state = state;
  engine.detail = detail || "";
  engine.since = Date.now();
  renderEngine();
}
```

`renderEngine()` writes `#engine-status` from a state map:
- `idle` → "R loads when you start working (~15 MB, one time)."
- `starting` → "Setting up R in the background… you can keep reading and
  typing." (+ detail, e.g. "downloading R", "installing packages",
  "loading the course data")
- `ready` → "R is ready ✓"
- `stalled` → "Setup is taking longer than usual — slow connection?
  Still working…" then, at the second threshold, append a
  `<button class="unlock-toggle" id="engine-retry">Retry setup</button>`
  and a link "or use <a href='getting-started.html'>Posit Cloud</a>".
- `failed` → "Could not load R in this browser. Your typing is safe; you
  can export anytime, retry below, or use Posit Cloud." + retry button.

Heartbeat: a 10s `setInterval` started when state is `starting`; if
`Date.now() - engine.since > 45000` show the first stalled message
(state stays `starting` — stall is a *display* condition, do not abort);
`> 120000` → show retry affordance. Any state change resets the clock.
Retry: clears `webRPromise = null`, sets `idle`, calls `ensureEngine()`.
Wire `#engine-retry` via event delegation on the panel (the button is
created dynamically).

Also: add `aria-live="polite"` to `#engine-status` in the six part pages
if not present.

### Step 2: Progress detail inside getWebR

Thread detail updates through the existing chain:

```js
setEngine("starting", "downloading R");
webRPromise = import(...).then(async function (mod) {
  var webR = new mod.WebR();
  await webR.init();
  await webR.evalRVoid("webr::shim_install()");
  if (CONFIG.prewarmPackages && CONFIG.prewarmPackages.length) {
    setEngine("starting", "installing packages (a few minutes the first time)");
    await webR.evalRVoid(
      "webr::install(c(" + CONFIG.prewarmPackages.map(function (p) {
        return '"' + p + '"';
      }).join(",") + "))"
    );
  }
  setEngine("starting", "loading the course data");
  await stageDataFiles(webR);
  setEngine("ready");
  return webR;
}).catch(function (err) { webRPromise = null; setEngine("failed"); throw err; });
```

Pre-installing via `webr::install` makes the assignment's own
`install.packages("tidyverse")` chunk a fast no-op (already installed),
so the student-visible chunk behavior is unchanged, just instant.

### Step 3: First-interaction trigger

```js
function ensureEngine() { getWebR().catch(function () { /* surfaced via state */ }); }
var prewarmed = false;
function armPrewarm() {
  if (prewarmed) { return; }
  prewarmed = true;
  ensureEngine();
}
```

After the document renders (end of the fetch `.then`), attach ONE-time
listeners: first `input` event anywhere in `#player-doc` (delegated),
first click on any `.run-code-button` already triggers `getWebR()` via
`runChunk` (unchanged), and `#student-name` input. Do NOT trigger on
scroll or page load. `{ once: true }` on the delegated listener or guard
with `prewarmed`.

### Step 4: Config + bumps

Add to `window.API209_PART` in part2–5.html:
`prewarmPackages: ["tidyverse"]`; part1.html: `prewarmPackages: []`.
Bump `assignment-player.js?v=` in all six player pages (hub has none).

**Verify each step**: `node --check docs/assignment-player.js`; then the
test plan.

## Test plan

Served site, fresh profile, DevTools network throttling for stall tests:
1. Open part2.html, do not touch anything → no webR network traffic.
2. Type one character in any answer box → status flips to "Setting up R in
   the background…", network shows webr.mjs; detail advances through
   packages → data → "R is ready ✓" with NO Run pressed.
3. After ready, press Run on the setup chunk → completes in seconds (no
   reinstall).
4. Throttle to ~1 Mbps, fresh profile, type → after 45s the
   stalled message appears; after 120s the Retry + Posit links appear;
   un-throttle, click Retry → reaches ready.
5. Block webr.r-wasm.org (DevTools request blocking), type → `failed`
   state with retry + Posit links; typing/export still work.
6. part1.html: prewarm runs with no package step (fast path to ready).
7. Run-before-ready: type (starts setup), immediately press Run → Run
   waits on the same promise and completes when ready (no double init,
   single webr.mjs request in the network log).

## Done criteria

- [ ] All seven test scenarios pass as described
- [ ] No webR traffic before first interaction on any part page
- [ ] `node --check` clean; no console errors in any scenario
- [ ] The literal strings for the five states appear in
      assignment-player.js (grep "Setting up R in the background")
- [ ] No commits; plans/README.md updated

## STOP conditions

- `webr::install("tidyverse")` errors in scenario 2 — capture the exact R
  error and report; do not ship a pre-warm that poisons the session.
- The drift check fails or plan 011 already landed and restructured
  getWebR — reconcile with its shared-runner module instead of editing in
  place.

## Maintenance notes

- Plan 011 consolidates this runner into a shared module used by the
  warm-up too — it must preserve the state machine contract
  (`setEngine` states and `#engine-status` rendering).
- If a future part needs more packages (e.g. ggrepel for Part 3's
  optional challenge), add to that page's `prewarmPackages` — do not
  globalize.
