# Plan 001: Unify the site on one counting model — "5 parts" everywhere

> **Executor instructions**: Follow this plan step by step. Run every
> verification command and confirm the expected result before moving to the
> next step. If anything in the "STOP conditions" section occurs, stop and
> report — do not improvise. When done, update the status row for this plan
> in `plans/README.md`.
>
> **Drift check (run first)**: `git diff --stat e2dfc1c..HEAD -- docs/`
> If any in-scope file changed since this plan was written, compare the
> "Current state" excerpts against the live code before proceeding; on a
> mismatch, treat it as a STOP condition.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW (static site, no build step; worst case is visual regression)
- **Depends on**: none
- **Category**: bug (UX/information architecture)
- **Planned at**: commit `e2dfc1c`, 2026-06-11

## Why this matters

Student feedback on the live site: *"i find it kind of hard to follow. e.g.
are there 5 parts or 7 parts?"* The site currently shows three competing
counting systems on the same screen: the hero progress card says "X **of 7**
done", the journey map shows **5 numbered circles** (where circle 3 is
"Assignment 1" but circle 4 is "Parts 2–5", so the numbers do not mean
parts), the roadmap says "**Five parts**", and the warm-up page counts
"0/**8**". After this plan, the only numbered things on the entire site are
**Parts 1–5** (the assignments). Setup and warm-up become an uncounted
prerequisite checklist, and the journey map uses named phases with no
numbers.

## Current state

This is a plain static site: hand-written HTML in `docs/`, one stylesheet
`docs/site.css`, one shared script `docs/site.js` (vanilla JS, IIFE, `var`
style, graceful no-JS degradation). It is served by GitHub Pages from the
`2026-refresh` branch, `/docs` folder. There is no build, lint, or test
command. Cache busting is done with query strings (`site.css?v=22`,
`site.js?v=2`) — bump both when you change those files.

Relevant files and the code as it exists today:

- `docs/site.js` — shared script. `STEPS` (lines ~17–100) is an ordered
  array of 7 trackable steps: `getting-started`, `warmup`, `part-1` …
  `part-5`. Display code that says "7":

  ```js
  // site.js:204 (inside injectProgressSummary)
  '<strong><span data-progress-count>0</span>/' + STEPS.length + "</strong>" +
  ```

  ```js
  // site.js:245-249 (inside renderNextCard)
  kicker.textContent = done === 0
    ? "Next step"
    : done === STEPS.length
      ? "Final step"
      : "Next step · " + done + " of " + STEPS.length + " done";
  ```

  ```js
  // site.js:319 (inside renderProgressSummary)
  bar.style.width = (done / STEPS.length) * 100 + "%";
  ```

  `renderJourney` (lines ~270–300) maps 5 journey DOM nodes to done-flags
  and rewrites `.journey-number` text to "✓" or the index+1.

- `docs/index.html` — homepage. The journey map at lines 89–135 is a
  `<nav class="journey-flow">` containing five `<a class="journey-step">`,
  each starting with `<span class="journey-number">N</span>` for N = 1..5
  and a `journey-kicker` (ORIENT / PRACTICE / WORK / BUILD / SUBMIT).
  Line 282: `<p class="eyebrow">Five parts</p>` above the roadmap.

- `docs/getting-started.html` — lines 67–73, a 5-item numbered
  `<ol class="lesson-path">`: Getting Started / Warm-up / Assignment 1 in
  Posit Cloud / Parts 2-5 / Submit on Canvas.

- `docs/interactive-hour.html` — warm-up page with its own inline script
  (bottom of file). Line 54: `<strong><span id="progress-count">0</span>/8</strong>`.
  The denominator comes from `progressBoxes.length` in the inline script
  (8 checkboxes: 7 sections with `data-progress` of `run-code`, `objects`,
  `functions`, `data`, `pipes`, `errors`, `ai`, plus the meta-checkbox
  `data-progress="finish"` "I completed the warm-up"). Lines 404–408: a
  3-item `lesson-path compact` (Getting Started / Warm-up / Assignment 1
  in Posit Cloud). The inline script's `updateProgress()` (around line
  553) computes `done` from `stepSections.filter(isSectionComplete)` and
  uses `progressBoxes.length` as denominator.

- `docs/site.css` — styles. `.journey-number` is a 42px crimson-bordered
  circle; `.journey-step.is-done .journey-number` turns it green.

Conventions to match: 2-space indent HTML, alphabetized CSS properties
within rules, `var`-based ES5-compatible JS in `site.js` (see existing
functions there as exemplars), `const`/arrow style in the
`interactive-hour.html` inline script. Keep all pages working with
JavaScript disabled: static HTML must read sensibly on its own.

## Commands you will need

| Purpose | Command | Expected on success |
|---------|---------|---------------------|
| Serve locally | `python3 -m http.server 8210 --directory docs` (run from repo root, in background) | site at http://localhost:8210 |
| Stale-count grep | `grep -rn "of 7\|/7<\|0</span>/8\|STEPS.length" docs/site.js docs/index.html docs/interactive-hour.html` | see per-step expectations |
| Anchor check | python snippet in Step 6 | `OK` |

## Scope

**In scope** (the only files you should modify):
- `docs/site.js`
- `docs/index.html`
- `docs/getting-started.html`
- `docs/interactive-hour.html`
- `docs/site.css`
- The `?v=` query strings for site.css/site.js in all `docs/*.html` files

**Out of scope** (do NOT touch):
- `assignments/*.Rmd` — assignment content is handled by plan 004.
- `docs/faq.html`, `docs/submission.html`, `docs/data.html` content
  (other than the `?v=` bump) — they contain no step counts.
- The localStorage key names (`api209-course-progress-v1`,
  `api209-warmup-progress-v2`, `api209-warmup-unlockall-v1`) — changing
  them wipes existing students' saved progress.
- The `STEPS` array *order and ids* in `site.js` — the next-step logic and
  saved progress depend on them.

## Git workflow

- Branch off `2026-refresh`: `advisor/001-unify-part-counting`
- Commit style: short imperative subject, e.g. "Unify site counting on five parts"
  (match `git log --oneline -10`)
- Do NOT push. Do NOT merge into `2026-refresh` — the site deploys from it.

## Steps

### Step 1: Split progress accounting in `site.js`

In `site.js`, add near the top (after `STEPS` / `SUBMIT_STEP`):

```js
var SETUP_IDS = ["getting-started", "warmup"];
var PART_IDS = ["part-1", "part-2", "part-3", "part-4", "part-5"];
```

Add two helpers next to `doneCount()`:

```js
function partsDone() {
  return PART_IDS.filter(function (id) { return progress[id]; }).length;
}
function setupDone() {
  return SETUP_IDS.filter(function (id) { return progress[id]; }).length;
}
```

Keep `doneCount()` and `nextStep()` unchanged (they drive ordering).

**Verify**: `node --check docs/site.js` → exit 0 (or
`python3 -c "print(open('docs/site.js').read().count('PART_IDS'))"` → ≥ 3 if node is unavailable).

### Step 2: Make the hero progress card count parts only

In `injectProgressSummary()` (site.js), replace the injected markup so it
shows a parts bar plus an uncounted setup line:

```js
wrap.innerHTML =
  '<div class="progress-row">' +
  '<span class="progress-label">Parts complete</span>' +
  '<strong><span data-progress-count>0</span>/5</strong>' +
  "</div>" +
  '<div class="progress-track" aria-hidden="true"><span data-progress-bar></span></div>' +
  '<p class="progress-setup" data-progress-setup></p>' +
  '<p class="progress-note">Progress is saved in this browser only. ' +
  'Mark parts complete in the roadmap below.</p>' +
  '<button type="button" class="progress-reset" data-progress-reset hidden>Reset my progress</button>';
```

In `renderProgressSummary()`, use `partsDone()` for the count and bar
(`/ 5`), and fill the setup line:

```js
var setupEl = document.querySelector("[data-progress-setup]");
if (setupEl) {
  setupEl.textContent = "Before Part 1: Getting started " +
    (progress["getting-started"] ? "✓" : "·") + "  Warm-up " +
    (progress.warmup ? "✓" : "·");
}
```

Show the reset button when `doneCount() > 0` (unchanged logic). In
`renderNextCard()`, change the kicker so the only number is parts:

```js
kicker.textContent = step === SUBMIT_STEP
  ? "Final step"
  : PART_IDS.indexOf(step.id) === -1
    ? "Before Part 1"
    : "Next up · " + partsDone() + " of 5 parts done";
```

(Note: `nextStep()` returns `SUBMIT_STEP` by reference when all steps are
done, so `step === SUBMIT_STEP` is a valid check.)

Add to `site.css` (in the "2026 refresh" section, matching alphabetized
properties):

```css
.progress-setup {
  color: var(--muted);
  font-size: 0.9rem;
  margin: 0 0 8px;
}
```

**Verify**: `grep -n "STEPS.length" docs/site.js` → no matches in
`injectProgressSummary`, `renderProgressSummary`, or `renderNextCard`
(matches may remain in `nextStep`/loop code). Serve locally, open
http://localhost:8210, confirm the hero panel reads "Parts complete 0/5"
with a "Before Part 1: …" line.

### Step 3: Replace the journey map's numbers with named phases

In `docs/index.html` lines 89–135, restructure the `journey-flow` from
five steps to **four**, removing the numeric circles' digits (keep the
`journey-number` span as a status circle, content empty by default):

1. kicker `Set up`, title `Getting Started`, href `getting-started.html` — keep current description.
2. kicker `Practice`, title `Warm-up`, href `interactive-hour.html` — keep current description.
3. kicker `Work`, title `Parts 1–5`, href `#assignments`, description:
   `Work through the five R Markdown assignments in order. Mark each part complete below.`
4. kicker `Submit`, title `Canvas`, href `submission.html` — keep current description.

Each step's first line becomes `<span class="journey-number" aria-hidden="true"></span>`.

In `site.js` `renderJourney()`, update the done-flag mapping for 4 nodes:

```js
var doneFlags = [
  Boolean(progress["getting-started"]),
  Boolean(progress.warmup),
  PART_IDS.every(function (id) { return progress[id]; }),
  false /* submission happens on Canvas; we cannot verify it here */
];
```

and set `number.textContent = isDone ? "✓" : "";` (no digits).

In `site.css`, give the empty circle a neutral resting state (the existing
`.journey-step.is-next` and `.is-done` styles already handle the other
states) and update `.journey-flow` from `repeat(5, …)` to
`repeat(4, minmax(0, 1fr))` in BOTH the base rule and any media-query
overrides that reference it.

**Verify**: `grep -c "journey-step" docs/index.html` → 4.
`grep -n 'journey-number">[0-9]' docs/index.html` → no matches.
In the served site, the journey shows 4 cards with empty circles; after
checking "Mark this part complete" on all five parts (roadmap section),
the third circle shows ✓.

### Step 4: Relabel the static lesson-path lists

`docs/getting-started.html` lines 67–73: replace the 5-item numbered list
with a 4-item list matching the journey phases (Set up → Warm-up →
Parts 1–5 → Submit on Canvas), replacing each `<span>N</span>` with the
phase kicker word, e.g.:

```html
<li class="is-current"><span>Set up</span><strong>Getting Started</strong></li>
<li><span>Practice</span><strong>Warm-up</strong></li>
<li><span>Work</span><strong>Parts 1–5 in Posit Cloud</strong></li>
<li><span>Submit</span><strong>Submit on Canvas</strong></li>
```

Update `.lesson-path` grid in `site.css` from `repeat(5, …)` to
`repeat(4, …)`. Apply the same relabeling to the 3-item
`lesson-path compact` in `docs/interactive-hour.html` lines 404–408
(Set up / Practice / Work — keep 3 items, keep `is-current` on Warm-up;
`.lesson-path.compact` already uses `repeat(3, …)`, leave its grid alone).

**Verify**: `grep -n "lesson-path" docs/getting-started.html docs/interactive-hour.html`
then visually confirm both lists show phase words, not digits.

### Step 5: Fix the warm-up count to 7 activities

In `docs/interactive-hour.html`:

- Line 54: change `0</span>/8` to `0</span>/7` and change the label
  `Your progress` (line ~53) to `Warm-up activities`.
- In the inline script's `updateProgress()`, count activities excluding
  the finish meta-section, and keep the celebration tied to everything:

```js
const activitySections = stepSections.filter((s) => s.id !== "finish");
const done = activitySections.filter(isSectionComplete).length;
count.textContent = done;
bar.style.width = `${(done / activitySections.length) * 100}%`;
const allDone = stepSections.every(isSectionComplete);
```

(The two `document.querySelector(".progress-panel"/".progress-complete")`
lines below keep using `allDone`.)

**Verify**: `grep -n "/8" docs/interactive-hour.html` → no matches in the
progress panel markup. In the browser: complete warm-up 1 (answer + check
box) → panel shows 1/7; completing all 7 + the final "I completed the
warm-up" checkbox still triggers the green celebration state.

### Step 6: Add a clarifying sentence, bump versions, check links

In `docs/index.html`, in the roadmap `section-heading` paragraph (below
the "Five parts" eyebrow, line ~282), prepend the sentence:
`These five parts are the whole assignment; everything else on this site supports them.`

Bump cache busters in ALL `docs/*.html` files: `site.css?v=22` → `?v=23`
and `site.js?v=2` → `?v=3`.

Run this anchor/link check from the repo root:

```bash
python3 - <<'EOF'
import re, os, sys
docs = 'docs'
ids = {}
links = []
for f in os.listdir(docs):
    if not f.endswith('.html'): continue
    html = open(os.path.join(docs, f)).read()
    ids[f] = set(re.findall(r'id="([^"]+)"', html))
    for href in re.findall(r'href="([^"]+)"', html):
        if href.startswith(('http', 'mailto:')): continue
        links.append((f, href))
bad = []
for src, href in links:
    page, _, frag = href.partition('#')
    page = page or src
    if page.endswith('.css') or page.endswith('.ico') or page.endswith('.js'): continue
    if page not in ids: bad.append((src, href, 'missing page')); continue
    if frag and frag not in ids[page]: bad.append((src, href, 'missing anchor'))
print('OK' if not bad else bad)
EOF
```

**Verify**: the script prints `OK`. (Known exception: FAQ `q-*` anchors are
JS-generated; if the script flags only `#q-…` fragments, that is
acceptable — list them and move on.)

## Test plan

No automated test infrastructure exists. Manual matrix, all on the locally
served site with a fresh browser profile (or after `localStorage.clear()`):

1. Fresh visit: hero shows "Parts complete 0/5", setup line shows two `·`,
   next-step card says "Before Part 1 / Getting Started".
2. Check the getting-started checkbox + complete the warm-up (or set
   `localStorage.setItem('api209-warmup-progress-v2','{"finish":true}')`,
   reload): setup line shows two ✓, next-step card says "Next up · 0 of 5
   parts done / Part 1…".
3. Mark parts 1–2 complete in the roadmap: bar at 40%, kicker "Next up · 2
   of 5 parts done", journey circles 1–2 ✓.
4. Mark all five parts: card flips to "Final step / Submit on Canvas",
   journey circle 3 ✓.
5. "Reset my progress" returns to state 1.
6. JS disabled: page shows static journey (4 phase cards), no progress
   panel, static next-step card pointing at Getting Started.

## Done criteria

- [ ] `grep -rn "of 7" docs/` → no matches
- [ ] `grep -n "0</span>/8" docs/interactive-hour.html` → no matches
- [ ] `grep -c "journey-step" docs/index.html` → `4`
- [ ] Anchor-check script prints `OK` (modulo JS-generated `q-*` anchors)
- [ ] All `docs/*.html` reference `site.css?v=23` and `site.js?v=3`
- [ ] Manual test matrix above passes
- [ ] `git status` shows only in-scope files modified
- [ ] `plans/README.md` status row updated

## STOP conditions

Stop and report back if:

- The excerpts in "Current state" don't match the live files (drift).
- Plan 005 (Part 5 staged release) has already landed and changed the
  roadmap markup — reconcile with its changes instead of overwriting.
- You find yourself wanting to rename localStorage keys or reorder
  `STEPS` — that breaks saved student progress; report instead.
- The journey-flow restructure requires touching `.journey-flow::before`
  (the connector line) in a way that breaks the mobile (max-width: 900px)
  vertical layout and you cannot fix it within `site.css` — report with a
  screenshot description.

## Maintenance notes

- Plan 005 (staged release of Part 5) adds a "release date" tag to the
  Part 5 roadmap article; it assumes the "5 parts" vocabulary from this
  plan. Land this plan first.
- Future reviewers: any new page or section that introduces a count
  ("X steps", "N modules") must reuse the part/phase vocabulary or extend
  it deliberately — this is exactly the drift that caused the original
  feedback.
- The hero panel's setup line uses "·" as the unchecked glyph; if a
  designer replaces it with an icon font or SVG, keep the `aria` text
  meaningful.
