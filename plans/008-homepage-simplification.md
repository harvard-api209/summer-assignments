# Plan 008: Rebuild the homepage around one action — the roadmap

> **Executor instructions**: Follow this plan step by step with its
> verifications. STOP conditions are binding. Update `plans/README.md`
> when done.
>
> **CRITICAL — working-tree state**: the in-browser assignment feature is
> UNCOMMITTED on `2026-refresh`. Work directly in the existing working
> tree. Do NOT branch from HEAD, do NOT commit, do NOT push.
>
> **Drift check (run first)**: plan 007 must already be DONE (check
> `plans/README.md` and `grep -c "posit-entry" docs/index.html` → 0). If
> 007 has not run, STOP.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW
- **Depends on**: plans/007-browser-first-ia.md
- **Category**: direction (teaching-team decision, grilled and approved)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12
- **Sign-off**: compressing the AI manifesto needs Dan's nod at review

## Why this matters

Faculty feedback called the site "hard to follow," and the homepage is
where that starts: nine sections, two AI essays 1,000px apart, a platform
map that browser-first made false, a journey block that duplicates the
roadmap, and a roadmap you cannot act on (it describes the five parts but
has no Open buttons). The teaching team approved this triage: the homepage
becomes hero → roadmap (the centerpiece, with Open buttons) →
how-it-works (3 bullets) → one merged AI section → help row. Roughly half
the length, every section answering a student question.

## Current state

`docs/index.html` (~400 lines), sections in order (verify ids/classes
before editing — 007 already removed `#posit-entry`):

1. `section.hero.course-dashboard` — hero copy + `aside.dashboard-panel`
   with the JS-rendered next-card + progress (KEEP).
2. `section.orientation-panel` with `#orientation-title` "How the summer
   assignments work" — contains `div.platform-map` (3 cards:
   Website/Posit/Canvas) and `nav.journey-flow` (4 phase cards) (CUT both).
3. `section.support#support` — 3-card support grid (KEEP, it is one row).
4. `section.manifesto#principles` — "Learning statistics with AI" +
   `div.principle-list` of 5 numbered principles (MERGE).
5. `section.split#data` — "One cleaned development snapshot", two
   paragraphs + dictionary button (COMPRESS to ~2 sentences + button).
6. `section.assignments#assignments` — roadmap: 5 `article.assignment`
   rows (kicker/h3/p, JS injects "Mark this part complete" checkboxes)
   (KEEP — PROMOTE: move directly under the hero, add Open buttons).
7. `section.split#ai` — "Debug, verify, explain" + check-list (CUT —
   merge its 4-item check-list into the manifesto section).

JS contracts that MUST keep working (from `docs/site.js`):
- `.next-card` with `span`/`h2`/`p`/`a.button` children (renderNextCard).
- `.dashboard-panel` (progress summary injection point).
- `article.assignment` with ids `part-1`…`part-5` (checkbox injection +
  is-done classes). Keep these ids and the `.assignment-list` wrapper.
- `renderJourney()` looks for `.journey-flow .journey-step` — after
  cutting the journey, ALSO delete `renderJourney` and its call inside
  `renderIndex()` in site.js, or it becomes dead code (it no-ops safely,
  but remove it cleanly and remove the `.journey-*` CSS rules).
- The nav "Assignments ▾" dropdown stays as is.

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Serve | `python3 -m http.server 8210 --directory docs` | :8210 |
| JS syntax | `node --check docs/site.js` | exit 0 |
| Dead-CSS sweep | `grep -n "journey-" docs/site.css docs/*.html` | no HTML hits after Step 2 |

## Scope

**In scope**: `docs/index.html`, `docs/site.js` (renderJourney removal +
roadmap "Open" links don't need JS — static), `docs/site.css`
(journey/platform-map rule removal + small additions), `?v=` bumps in all
`docs/*.html`.

**Out of scope**: all other pages (007 handled them); auto-progress
badges (plan 009 — leave clean hooks, do not implement detection here);
the player.

## Git workflow

Working tree only. NO commits, NO pushes.

## Steps

### Step 1: New section order and hero

Reorder `index.html` body to: hero → assignments roadmap → how-it-works →
AI section → support → data line → footer. Hero copy tightens to ~2
sentences: five coding assignments before Math Camp; do them right here in
the browser; download each finished `.Rmd` and submit on Canvas. Below the
hero-text add one quiet line:
`<p class="hero-aside">New to coding? <a href="interactive-hour.html">Take the optional 20-minute warm-up</a> first.</p>`
(style `.hero-aside` muted, 0.95rem). The dashboard panel stays untouched.

**Verify**: served homepage shows roadmap immediately after hero.

### Step 2: Cut platform map + journey; clean JS/CSS

Delete the whole `section.orientation-panel` (platform map + journey-flow
+ its heading). In `docs/site.js`: delete `renderJourney()` and its call
in `renderIndex()`. In `docs/site.css`: delete the `.journey-flow`,
`.journey-step`, `.journey-number`, `.journey-kicker` rule blocks AND
their entries in the two `@media` blocks; delete `.platform-map` rules
ONLY IF no other page uses them — `grep -l "platform-map" docs/*.html`
first (getting-started.html still uses it → keep the CSS in that case).

**Verify**: `node --check docs/site.js` exit 0; grep table above; homepage
console shows no errors; marking a part complete still updates the
next-card and progress bar (journey removal must not break renderIndex).

### Step 3: Make the roadmap actionable

In each `article.assignment` (ids part-1…part-5) add, after the `<p>`:
`<a class="button secondary assignment-open" href="partN.html" target="_blank" rel="noopener">Open Part N</a>`.
Add CSS `.assignment-open { grid-column: 3; justify-self: start; margin-top: 10px; }`
(and `grid-column: 1` inside the ≤900px media block, matching
`.course-check`). Keep the JS-injected checkbox working — it appends to
the same article; confirm visual order (button above checkbox is fine).
Update the section heading copy: drop "There are no separate assignment
pages on this website" (now false) — say each part opens in its own tab,
works in the browser, and exports the `.Rmd` you submit.

**Verify**: each roadmap card has a working Open button; checkbox still
toggles "Parts complete N/5".

### Step 4: How-it-works strip (new, small)

After the roadmap, a 3-card `note-grid` (existing component):
1. `Work` / "Do each part on this site" — runnable chunks, autosaves in
   your browser.
2. `Download` / "Export your .Rmd" — your backup and your submission file;
   fix errors/warnings before downloading.
3. `Submit` / "Upload to Canvas" — link submission.html.

### Step 5: Merge the two AI sections

Replace `section.manifesto#principles` content and delete
`section.split#ai`. The merged section keeps: the eyebrow/heading
("Learning statistics with AI"), ONE lede paragraph (use the existing
first manifesto paragraph: "These assignments are not about avoiding
AI… without becoming a crutch."), the 5-item `principle-list` verbatim
(course stance — do not rewrite the principles' text), and fold the
"Debug, verify, explain" check-list's 4 items as a compact
`ul.check-list` titled "In practice". Everything else from both sections
is cut.

**Verify**: exactly one section on the page mentions AI;
`grep -c "Debug, verify, explain" docs/index.html` → 0; the five
principles' text is byte-identical to before (diff against git HEAD copy
is fine for this section since index.html at HEAD predates… NOTE:
index.html IS tracked with uncommitted modifications; compare against the
current working-tree text you started from, e.g. keep a copy at
/tmp/index-before.html).

### Step 6: Compress the data section + bump

Data section → heading, two sentences (same dataset all five parts; each
row = one country-year; sources WDI/WGI), and the existing
"Browse the data dictionary" button. Bump `?v=` (css and js +1) across all
`docs/*.html`.

**Verify**: full-page read-through ≤ ~half the previous scroll length;
link checker from plan 007 Step 7 prints `OK`.

## Test plan

Fresh-profile manual matrix: (1) first visit: hero CTA → Getting…
no-setup path straight into Part 1; (2) roadmap Open buttons open part
pages in new tabs; (3) mark parts complete → next-card/progress update;
(4) reset progress works; (5) JS off: static homepage still shows hero,
roadmap with Open buttons, sections in order; (6) mobile 375px: roadmap
cards stack, buttons full-width-ish, no horizontal scroll.

## Done criteria

- [ ] Section order: hero → roadmap → how-it-works → AI → support → data
- [ ] platform-map + journey-flow markup gone from index.html; site.js has
      no renderJourney; no `journey-` classes referenced by any HTML
- [ ] One AI section; principles text unchanged
- [ ] Roadmap cards each have Open buttons; checkboxes still work
- [ ] `node --check docs/site.js` exit 0; no console errors
- [ ] No commits; plans/README.md updated

## STOP conditions

- Plan 007 not done (drift check).
- Removing the journey breaks renderIndex (console error) and the fix
  isn't a clean deletion — report rather than patching around it.
- You are tempted to implement progress *detection* (reading
  api209-partN-work-v1) — that is plan 009.

## Maintenance notes

- Plan 009 will add "In progress" badges to these roadmap cards and a
  smarter hero CTA — it assumes the Step 3 card structure (`#part-N`
  articles with `.assignment-open` buttons).
- If Dan wants the AI manifesto restored to long form, only Step 5
  reverses; keep its diff isolated in review.
