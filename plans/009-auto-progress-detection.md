# Plan 009: Auto-detect "in progress" from player state; keep completion manual

> **Executor instructions**: Follow steps + verifications; STOP conditions
> binding; update `plans/README.md` when done.
>
> **CRITICAL — working-tree state**: the player feature is UNCOMMITTED on
> `2026-refresh`. Work in the existing working tree. NO commits/pushes.
>
> **Drift check (run first)**: plans 007 and 008 are DONE per
> `plans/README.md`; `docs/index.html` roadmap articles `#part-1`…`#part-5`
> exist with `.assignment-open` buttons. Otherwise STOP.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW (read-only over existing localStorage; no schema changes)
- **Depends on**: plans/008-homepage-simplification.md
- **Category**: direction (grilled decision: option b)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12

## Why this matters

Progress is currently manual: a student doing Part 2 in the browser must
separately remember to tick a checkbox on the homepage, so the roadmap can
say "0/5" while Part 3 is half-done — a disconnect that makes the
dashboard feel dead and unreliable. The player already stores per-part
work in localStorage on the same origin. Decision (grilled, approved):
auto-detect and display **in progress** (with counts), make the hero CTA
"Continue Part N", and keep **completion** a deliberate manual checkbox —
because export ≠ done and only the student knows when they're finished.

## Current state

- Player storage (written by `docs/assignment-player.js`, keys defined per
  part page in `window.API209_PART.storageKey`):
  `api209-part1-work-v1` … `api209-part5-work-v1`, JSON:
  `{ name, chunks: {editIdx: code}, answers: {answerIdx: text}, savedAt }`.
  `chunks` holds ONLY chunks the student edited away from the default;
  `answers` holds typed answers (possibly empty strings after deletes).
- Course progress (manual): `api209-course-progress-v1`,
  `{ "part-1": true, … , "getting-started": true, warmup: true }`, owned
  by `docs/site.js` (`PART_IDS`, `partsDone()`, `renderNextCard()`,
  `renderAssignments()`, `renderProgressSummary()` — all in the index-only
  section guarded by `initIndex()`).
- Roadmap DOM (after 008): `article.assignment#part-N` containing kicker
  div, `h3`, `p`, `a.assignment-open`, JS-appended
  `label.done-check.course-check` checkbox.
- `renderNextCard()` picks the first incomplete step from `STEPS` and
  writes kicker/h2/p/a of `.next-card`. Each part's total answer count is
  NOT known to site.js (it lives in the part's Rmd). Hardcode totals in a
  map (cheap, content-stable): part-1: 7 answers, part-2/3/4: count the
  `[Write your … here.]` placeholders in
  `docs/web-assignments/part-N.Rmd` at implementation time
  (`grep -c "^\[Write your" docs/web-assignments/part-N.Rmd`) and inline
  the numbers with a comment saying how to recount.

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Serve | `python3 -m http.server 8210 --directory docs` | :8210 |
| JS syntax | `node --check docs/site.js` | exit 0 |
| Answer totals | `grep -c "^\[Write your" docs/web-assignments/part-{1,2,3,4,5}.Rmd` | five numbers for the map |

## Scope

**In scope**: `docs/site.js`, `docs/site.css` (badge styles), `?v=` bumps.
**Out of scope**: `docs/assignment-player.js` (no storage schema changes —
the reader adapts to the writer, never the reverse), all HTML except `?v=`.

## Steps

### Step 1: Add a player-state reader to site.js

Next to `PART_IDS`, add:

```js
var PART_ANSWER_TOTALS = { 1: 7, 2: N2, 3: N3, 4: N4, 5: N5 }; /* recount:
  grep -c "^\[Write your" docs/web-assignments/part-N.Rmd */

function partWork(n) {
  try {
    var raw = localStorage.getItem("api209-part" + n + "-work-v1");
    if (!raw) { return null; }
    var w = JSON.parse(raw) || {};
    var answers = 0;
    Object.keys(w.answers || {}).forEach(function (k) {
      if (String(w.answers[k]).trim()) { answers += 1; }
    });
    var chunks = Object.keys(w.chunks || {}).length;
    if (!answers && !chunks) { return null; }
    return { answers: answers, chunks: chunks, total: PART_ANSWER_TOTALS[n] || 0 };
  } catch (err) {
    return null;
  }
}
```

**Verify**: `node --check docs/site.js` exit 0.

### Step 2: In-progress badges on the roadmap

In `renderAssignments()` (currently toggles `is-done` per article), also
manage a status line: for each part N not marked complete, if
`partWork(N)` is non-null, ensure a
`<p class="assignment-status">In progress · {answers} of {total} answers written</p>`
exists right after the article's `h3` (create once, update text; remove it
when null or when the part is marked complete — completed articles already
show "· Done ✓" via the kicker). If `total` is 0 fall back to
"In progress". CSS: `.assignment-status { color: var(--accent); font-size: 0.88rem; font-weight: 600; margin: 6px 0 0; }`.

**Verify**: in the served homepage console:
`localStorage.setItem('api209-part2-work-v1', JSON.stringify({answers:{0:'x',1:'y'},chunks:{}}))`,
reload → Part 2 card shows "In progress · 2 of N2 answers written"; mark
Part 2 complete → badge disappears, Done ✓ shows.

### Step 3: Smarter hero CTA

In `renderNextCard()`: after computing `step`, if the step is a part
(`PART_IDS.indexOf(step.id) !== -1`) and `partWork(partNumber)` is
non-null, set kicker to
`"Continue · " + partsDone() + " of 5 parts done"`, h2 unchanged, CTA text
`"Continue Part N"` (same href). Additionally — pick the *right* part to
continue: before falling back to `nextStep()`'s first-incomplete order,
prefer the lowest-numbered incomplete part that has work
(`partWork` non-null). Keep the existing setup-steps behavior ("Before
Part 1") untouched.

**Verify**: with the Step 2 test state (no parts complete, part 2 has
work, part 1 untouched): hero shows "Continue Part 2". Clear part 2's
key → hero shows "Part 1 … Start".

### Step 4: Progress-summary nuance + bump

In `renderProgressSummary()`'s setup line area, append after the
parts-complete bar a muted one-liner when any incomplete part has work:
`<p class="progress-live" data-progress-live></p>` injected with text like
"In progress: Part 2, Part 4" (comma list). Bump `?v=` for site.css/site.js
across all `docs/*.html`. Clean test keys from your browser when done.

**Verify**: full manual matrix below.

## Test plan

Console-seeded states on the served homepage (clear storage between):
1. Nothing → "Part 1 / Start", no badges.
2. part1 work only → "Continue Part 1", Part 1 badge.
3. part1 complete (checkbox) + part3 work → CTA "Continue Part 3"; Part 1
   shows Done ✓ and NO badge; Part 3 badge with counts.
4. All five complete → "Final step / Submit on Canvas" (existing behavior
   unchanged).
5. Corrupted JSON in a work key (`localStorage.setItem('api209-part2-work-v1','{{')`)
   → no crash, card renders as untouched (partWork returns null).
6. JS disabled → static roadmap, no badges, no errors.

## Done criteria

- [ ] All six test states pass
- [ ] `node --check docs/site.js` exit 0; zero console errors
- [ ] No changes to assignment-player.js or any storage write path
- [ ] Completion remains exclusively the manual checkbox
- [ ] No commits; plans/README.md updated

## STOP conditions

- The player's storage schema differs from "Current state" (open one real
  key and compare) — reconcile the reader, never migrate the writer here.
- You find yourself wanting to auto-complete on download — explicitly
  rejected in design (export ≠ done).

## Maintenance notes

- PART_ANSWER_TOTALS must be recounted if assignment content changes —
  plan 012's regeneration script should print the counts as a reminder
  (note added there).
- If a future part page changes its storageKey, this reader silently
  shows "untouched" — keep keys stable or update both sides.
