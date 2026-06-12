# Plan 011: Harden the assignment player (honest autosave, pinned webR, sanitized export, validated parse)

> **Executor instructions**: Follow steps + verifications; STOP conditions
> binding; update `plans/README.md` when done.
>
> **CRITICAL — working-tree state**: the player is UNCOMMITTED on
> `2026-refresh`. Work in the existing working tree. NO commits/pushes.
>
> **Drift check (run first)**: confirm the excerpts in "Current state"
> against `docs/assignment-player.js` (line numbers may have shifted if
> plan 010 ran first — re-locate by searching the quoted code; if the
> quoted code is gone entirely, STOP).

## Status

- **Priority**: P2
- **Effort**: M
- **Risk**: LOW-MED (touches autosave and export; every change has a
  direct test)
- **Depends on**: plans/010-prewarm-engine-status.md (same file; run 010
  first so this plan integrates with the state machine, not the old
  status string)
- **Category**: bug (vetted audit findings #2, #3, #6, #7, #8, #9)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12

## Why this matters

Five audit-confirmed weaknesses in the code that carries graded student
work: (1) when localStorage is full, autosave fails silently while the
panel keeps saying "Saved ✓" — the worst kind of lie for an anxious
student; (2) webR loads from an unpinned `/latest/` CDN URL in two files,
so the graded-work runtime can change under us overnight; (3) a student
name containing a newline or quote corrupts the exported YAML, and a `/`
breaks the download filename; (4) `parseRmd` renders malformed input
silently wrong instead of failing visibly; (5) the warm-up page carries a
second, divergent copy of the webR runner. Plus the durability-copy
finding: students fear losing browser work and the pages under-promise
what autosave does.

## Current state

`docs/assignment-player.js` (search-anchor the lines):

```js
// scheduleSave, ~:43-59 — silent failure + status from a boot-time flag
try { state.savedAt = ...; localStorage.setItem(CONFIG.storageKey, JSON.stringify(state)); }
catch (err) { /* best effort */ }
var el = document.getElementById("autosave-status");
if (el) { el.textContent = storageOk ? "Saved in this browser ✓" : "⚠ ..."; }
```

```js
// getWebR, ~:349 — unpinned
webRPromise = import("https://webr.r-wasm.org/latest/webr.mjs")
```

```js
// buildRmd, ~:470 — name into YAML with only double-quote replacement
yaml = yaml.replace('author: "YOUR NAME"', 'author: "' + name.replace(/"/g, "'") + '"');
// downloadRmd — filename: CONFIG.exportBase + " - " + name + ".Rmd" (no sanitizing)
```

`docs/interactive-hour.html:~705-760` — its own `getWebR()` +
run-button handler (no graphics fallback, no shim, separate status), used
by the 4 warm-up run buttons.

`parseRmd` (~:65-130) — no validation: an unclosed fence swallows the rest
of the document into one chunk; missing closing `---` makes the whole file
YAML.

Durability copy: `docs/assignments.html` "Autosave, per browser" card and
each part page's `#autosave-status` line — accurate but fear-inducing
without the full promise (survives restarts; clearing browser data
deletes; download = durable backup).

webR version to pin: resolve the current release at implementation time
via `curl -sI https://webr.r-wasm.org/latest/webr.mjs` redirects or the
docs at https://docs.r-wasm.org — use the versioned URL form
(e.g. `https://webr.r-wasm.org/v0.4.2/webr.mjs`); verify the chosen
version actually serves webr.mjs before wiring it in.

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Serve | `python3 -m http.server 8210 --directory docs` | :8210 |
| JS syntax | `node --check docs/assignment-player.js` | exit 0 |
| Unpinned grep | `grep -rn "webr.r-wasm.org/latest" docs/` | 0 matches when done |

## Scope

**In scope**: `docs/assignment-player.js`, a NEW `docs/webr-runner.js`
(shared module), `docs/interactive-hour.html` (swap inline runner for the
shared one), `docs/assignments.html` + part pages (durability copy, script
tag for the shared module), `?v=` bumps.

**Out of scope**: storage schema changes (plan 009 reads it as-is);
homepage; any R/Rmd content.

## Steps

### Step 1: Truthful autosave status

In `scheduleSave`: track the outcome per save —

```js
var ok = false;
try { ...setItem...; ok = true; } catch (err) { ok = false; }
var el = document.getElementById("autosave-status");
if (el) {
  el.textContent = !storageOk
    ? "⚠ This browser view cannot save between visits — export your .Rmd before closing."
    : ok
      ? "Saved in this browser ✓"
      : "⚠ Could not save — browser storage is full. Download your .Rmd NOW to keep this work.";
}
```

**Verify**: in the served part1 console, fill storage to quota
(`try { localStorage.setItem('x','y'.repeat(5e6)) } catch(e){}` repeatedly
or set a 4.9 MB filler), type in a chunk → panel shows the storage-full
warning, not "Saved ✓". Remove filler → typing again returns to Saved ✓.

### Step 2: Pin webR in one shared runner module

Create `docs/webr-runner.js` exposing (as a plain global,
`window.API209WebR`, matching the site's no-modules convention — script
tag, IIFE):
`{ ensure(config), run(code, ui), resetSession(), getState() }` — move
`getWebR`/`setEngine`/`renderEngine`/heartbeat/`stageDataFiles`/`runChunk`'s
shelter+captureGraphics core out of assignment-player.js into it, with the
pinned URL in exactly one place:

```js
var WEBR_URL = "https://webr.r-wasm.org/vX.Y.Z/webr.mjs"; /* pinned — see
   plans/011: verify a new version on a part page before bumping */
```

assignment-player.js keeps its parse/render/save/export logic and calls
the runner. The warm-up's inline `getWebR`/run-button handler
(`interactive-hour.html` bottom script) is replaced by calls to the same
runner (it has no `#engine-status`; pass a null-safe ui object — the
runner must not assume panel elements exist). Add
`<script src="webr-runner.js?v=1" defer></script>` before the consumers
on the six player pages + the warm-up.

**Verify**: unpinned grep → 0; part1 run prints `[1] 4`-style output as
before; warm-up run buttons still work (all four), including the NA
example; `node --check docs/webr-runner.js` exit 0.

### Step 3: Sanitize the name at the export boundary

```js
function safeName(raw) {
  return raw.replace(/[\r\n"\\]/g, " ").replace(/\s+/g, " ").trim();
}
function safeFilePart(raw) {
  return safeName(raw).replace(/[\/:*?<>|]/g, "-");
}
```

Use `safeName` in the YAML author replacement and `safeFilePart` in the
filename. Do NOT mutate `state.name` itself (display keeps what they
typed).

**Verify**: set name to `Ana "O'Hara"/\nSmith` in the panel; buildRmd()
in console → YAML author line is one line, no `"` inside the quotes;
download filename contains no `/`.

### Step 4: Validate parses, fail visibly

After `parseRmd` in the boot `.then`, add a sanity gate:

```js
function validateParse(p, raw) {
  var problems = [];
  if (!p.yaml || p.yaml.indexOf("title:") === -1) { problems.push("missing YAML"); }
  var fences = (raw.match(/^```/gm) || []).length;
  if (fences % 2 !== 0) { problems.push("unbalanced code fences"); }
  if (!p.blocks.some(function (b) { return b.type === "chunk"; })) { problems.push("no R chunks"); }
  return problems;
}
```

If problems is non-empty: render the loading element as a visible error
("This assignment didn't load correctly (reason). Reload the page; if it
persists, use Posit Cloud and tell the teaching team.") and DO NOT render
a half-document (a wrong render risks misaligned exports).

**Verify**: temporarily serve a corrupted copy (delete one closing fence
in /tmp mirror copy of part-1.Rmd) → error message, no chunks rendered;
restore → normal render.

### Step 5: Durability copy + unexported-changes guard

- `docs/assignments.html` "Autosave, per browser" card → add the explicit
  promise: survives restarts on this device; clearing browser data or
  switching devices loses it; the downloaded `.Rmd` is the durable copy.
- Track `lastExportedAt` and a dirty flag in the player (set dirty in
  `scheduleSave`, clear in `downloadRmd`). Add a `beforeunload` handler
  that warns ONLY when (dirty && answers-or-chunks non-empty &&
  never-exported-this-session). Keep it conservative — constant nagging
  trains students to ignore it.
- Panel note under the download button: "Last downloaded: never / HH:MM".

**Verify**: type, try closing the tab → browser confirm appears; download,
close → no confirm. Reload → "Last downloaded" persists?? — NO: keep
lastExportedAt session-only (simpler, honest); verify it resets per
session and the note says "not yet in this session".

### Step 6: Bump + full regression pass

Bump `?v=` for assignment-player.js (+1) and add webr-runner.js?v=1 across
the six player pages and the warm-up. Re-run the core regression: part1
export-with-no-edits is byte-identical to `docs/web-assignments/part-1.Rmd`
(console: fetch source, compare to `api209Player.buildRmd()` after
clearing storage); chunk run; answer autosave across reload.

## Test plan

The verifications above, plus: warm-up full pass (4 run buttons, lock
behavior unchanged); part2 prewarm still works end-to-end (plan 010's
scenario 2) — the runner refactor must not regress it.

## Done criteria

- [ ] Storage-full scenario shows the warning; normal saves show ✓
- [ ] `grep -rn "webr.r-wasm.org/latest" docs/` → 0 (pinned in exactly one file)
- [ ] Name-sanitization console checks pass
- [ ] Corrupted-source scenario fails visibly, renders nothing editable
- [ ] beforeunload fires only in the unexported-work case
- [ ] Export round-trip still byte-identical on a clean profile
- [ ] `node --check` clean on both JS files; no console errors
- [ ] No commits; plans/README.md updated

## STOP conditions

- Excerpts don't match (drift) — re-anchor by searching the quoted code.
- The runner extraction would require changing the warm-up's quiz/locking
  logic — out of scope; only its webR calls move.
- The pinned webR version fails any existing scenario (graphics capture,
  shim_install) — report the version matrix rather than shipping a
  downgrade.

## Maintenance notes

- Bumping the pinned webR version = change one constant in
  webr-runner.js, then run plan 010's scenario list before pushing.
- The beforeunload guard intentionally stays session-scoped; revisit only
  with real student feedback.
