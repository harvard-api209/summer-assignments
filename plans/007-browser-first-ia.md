# Plan 007: Make the browser path the official default across the whole site

> **Executor instructions**: Follow this plan step by step. Run every
> verification command and confirm the expected result before moving on. If
> anything in "STOP conditions" occurs, stop and report. When done, update
> this plan's row in `plans/README.md`.
>
> **CRITICAL — working-tree state**: the in-browser assignment feature
> (assignment-player.js, part1–5.html, assignments.html, web-assignments/)
> is UNCOMMITTED on branch `2026-refresh`. Work directly in the existing
> working tree. Do NOT create a branch from HEAD (it would lack these
> files), do NOT commit, do NOT push — the teaching team reviews locally
> first.
>
> **Drift check (run first)**: confirm `docs/part1.html` and
> `docs/assignment-player.js` exist and `git status --short docs/ | wc -l`
> ≥ 10. If the player files are missing, STOP — the feature was reverted.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW (copy/IA changes on a static site)
- **Depends on**: none (do this FIRST; 008/009 build on its framing)
- **Category**: direction (teaching-team decision, grilled and approved)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12
- **Sign-off**: browser-first as official policy and warm-up demotion need
  Dan's nod before this goes live (build now; flag at review)

## Why this matters

The teaching team decided: **the browser is the official default path**
(assignments run in-page via webR; no account, no setup), and Posit Cloud
is the optional alternative. Six pages still say the opposite — the
Getting Started page is titled "Work in Posit Cloud" and presents account
setup as step 1, the warm-up's finish button sends students only to Posit,
the submission page assumes a Files pane, and the FAQ leads with Posit
account problems. Every contradiction is a real student asking "wait,
which one am I supposed to use?" This plan makes every page tell the same
story: *work on this website → download your .Rmd → submit to Canvas;
Posit Cloud if you prefer RStudio.*

## Current state

Static site in `docs/` (hand-written HTML, vanilla JS in `docs/site.js`,
no build). Cache busting via `?v=` query strings on site.css (currently
`v=26`) and site.js (`v=6`) — bump both in ALL `docs/*.html` when you
change those files. Conventions: 2-space-indent HTML; `var`-style ES5 JS
in site.js. All pages share a hand-copied header nav:

```html
<!-- every docs/*.html, in <header> -->
<a href="index.html">Home</a>
<a href="getting-started.html">Getting started</a>
<a href="interactive-hour.html">Warm-up</a>
<details class="nav-dropdown"> … Assignments dropdown … </details>
<a href="submission.html">Submission</a>
<span class="nav-divider" aria-hidden="true"></span>
<a href="faq.html">FAQ</a>
<a href="data.html">Data</a>
```

Key offending locations (verify each before editing):

- `docs/getting-started.html:49` — `<h1 id="getting-started-title">Work in Posit Cloud</h1>`;
  the page presents Posit setup as the universal first step.
- `docs/interactive-hour.html:450` — lesson path item
  `<li><span>Work</span><strong>Parts 1&ndash;5 in Posit Cloud</strong></li>`;
  `:467` — finish button `Open your saved copy in Posit Cloud` is the only
  primary action after the warm-up.
- `docs/submission.html:57` — `<h2 id="before-submit-title">Before downloading</h2>`
  section assumes Posit Cloud + Files pane; there are no browser-path
  submission instructions on the page.
- `docs/index.html:83` — platform map card "Posit Cloud / Do the work";
  `:141` — `section id="posit-entry"` ("First visit vs. returning"), pure
  Posit content. (Plan 008 rebuilds the homepage; this plan only MOVES the
  posit-entry content out — leave the rest of index.html to 008.)
- `docs/faq.html` — section order: Access and setup (all Posit) first; no
  browser-player section exists. Meta descriptions on index/getting-started
  say "in Posit Cloud" only.

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Serve locally | `python3 -m http.server 8210 --directory docs` (repo root) | site on :8210 |
| Posit-primacy grep | `grep -rn "Work in Posit Cloud\|in Posit Cloud</strong>\|saved copy in Posit Cloud" docs/*.html` | empties as you go |
| Link/anchor check | script in Step 7 | `OK` (modulo JS-generated `q-*`) |
| JS syntax | `node --check docs/site.js` | exit 0 |

## Scope

**In scope**: `docs/getting-started.html`, `docs/interactive-hour.html`,
`docs/submission.html`, `docs/faq.html`, `docs/index.html` (ONLY the
posit-entry section move), all `docs/*.html` nav blocks + meta
descriptions + `?v=` bumps, `docs/site.js` (one STEPS blurb).

**Out of scope**: homepage restructure beyond the posit-entry move (plan
008); `docs/assignment-player.js` (plans 010/011); anything outside
`docs/`; renaming files (keep `getting-started.html` as the filename so no
inbound links break — only its content/title change).

## Git workflow

Work directly on the `2026-refresh` working tree. NO commits, NO pushes.

## Steps

### Step 1: Reframe getting-started.html as "Using Posit Cloud (optional)"

- `<title>` → `API-209 Posit Cloud Setup (Optional)`; meta description →
  "Optional: set up Posit Cloud to do the API-209 coding assignments in
  RStudio instead of the browser."
- Hero: eyebrow `Optional setup`, h1 `Using Posit Cloud (optional)`,
  hero-text explaining: assignments run in the browser by default with
  nothing to install — this page is for students who prefer RStudio, and
  it becomes useful again at Math Camp when everyone moves to RStudio.
- Remove the "Recommended path" lesson-path section entirely (the page is
  no longer a step on the path).
- Keep everything else (first steps, screenshots, Good to know grid,
  folder structure, panes, tools) — that content is correct for the Posit
  path.
- ADD (from index.html) the "First visit vs. returning" two-card section:
  cut the whole `<section class="orientation-panel" id="posit-entry">…</section>`
  from `docs/index.html:141` and paste it (verbatim, keeping the
  `entry-split` markup) after the First steps section here.

**Verify**: `grep -c "entry-split" docs/getting-started.html` → ≥ 1;
`grep -c "posit-entry" docs/index.html` → 0; served page reads as an
optional guide.

### Step 2: Update every nav

In ALL `docs/*.html` (12 files incl. part1–5): replace
`<a href="getting-started.html">Getting started</a>` with nothing in the
main cluster, and add `<a href="getting-started.html">Posit Cloud</a>`
into the support cluster after FAQ, i.e. final nav order:
`Home · Warm-up · Assignments ▾ · Submission | FAQ · Data · Posit Cloud`.
Use one perl/python pass so all 12 files stay identical.

**Verify**: `grep -c 'getting-started.html">Posit Cloud' docs/*.html` → 1
per file (12 total);
`grep -c 'getting-started.html">Getting started' docs/*.html | grep -v ':0'` → empty.

### Step 3: Warm-up page — optional framing + browser-first finish

In `docs/interactive-hour.html`:
- Hero eyebrow `Before Assignment 1` → `Optional warm-up`; add one
  hero-text sentence: "Already comfortable running a line of code? Skip
  straight to <a href='part1.html'>Part 1</a> — you can come back any
  time."
- Lesson path (~line 448–451): change
  `<strong>Parts 1&ndash;5 in Posit Cloud</strong>` →
  `<strong>Parts 1&ndash;5 on this site</strong>`.
- Finish section (~line 455–470): replace the primary button
  (`Open your saved copy in Posit Cloud`) with
  `<a class="button primary" href="part1.html">Start Part 1 in your browser</a>`
  and make the Posit link a secondary button
  `<a class="button secondary" href="getting-started.html">Prefer RStudio? Posit Cloud setup</a>`.
  Update the finish copy: "open Part 1 right here on the site" instead of
  "open the assignment file in Posit Cloud".

**Verify**: `grep -n "saved copy in Posit Cloud" docs/interactive-hour.html`
→ no matches; finish primary button hrefs to part1.html.

### Step 4: Submission page — browser path first

Restructure `docs/submission.html` body into two clearly labeled paths,
browser first:

1. New section `## If you worked in your browser` (before the current
   "Before downloading"): you already downloaded your `.Rmd` from the
   assignment page (the Download button checks for errors/warnings); make
   sure your name is in the panel before downloading so the file is named
   `R Summer Assignment N - First Last.Rmd`; then submit that file on
   Canvas. Include a "haven't downloaded yet?" link row to part1–5 pages.
2. Demote the existing Posit content under a heading
   `## If you worked in Posit Cloud` (keep the checklist, Files-pane SVG
   figure, and steps as they are beneath it).
3. Hero text: replace "Do not submit knitted HTML…" framing with
   path-neutral copy: one `.Rmd` per part, same file from either path.

**Verify**: served page shows browser section above Posit section; grep
`"If you worked in your browser"` → 1.

### Step 5: FAQ — add a browser section, reorder for browser-first

In `docs/faq.html`:
- New section (placed FIRST, before "Access and setup"), heading
  `<h2 id="browser-faq-title">Working in the browser</h2>`, with a
  `faq-list` of five articles (write answers in the site's plain, direct
  voice; key facts in parentheses must be preserved):
  1. "Where is my work saved?" (in this browser on this device,
     automatically as you type; survives restarts; clearing browser data
     deletes it; the downloaded `.Rmd` is the durable backup)
  2. "I lost my work in the browser — what now?" (different
     browser/device/private-window = different storage; check the same
     browser; re-import is manual: open your last downloaded `.Rmd` and
     paste back)
  3. "Why is the first Run in Parts 2–5 slow?" (R plus the tidyverse
     download into the browser once, a few minutes; later runs are fast;
     Posit Cloud is quicker for the data-heavy parts if you prefer)
  4. "Is the browser version the same assignment as Posit Cloud?" (yes —
     same questions, same exported `.Rmd`, graded identically)
  5. "Do I need Posit Cloud at all?" (no for the summer; optional; Math
     Camp will move everyone to RStudio later — link getting-started.html)
- Retitle the "Access and setup" h2 to `Posit Cloud: access and setup`.
- Add the new section to the `faq-jump` chips row (first chip).
- Meta description: "…coding assignments in your browser or Posit Cloud."

**Verify**: chips row has 8 links; new section renders; FAQ filter finds
"browser" (type in the filter → ≥ 4 matches).

### Step 6: Sweep remaining copy + site.js

- Meta descriptions in `docs/index.html` and any page saying only "in
  Posit Cloud" → "in your browser or in Posit Cloud".
- `docs/site.js` STEPS entries for part-2…part-5 (around lines 57–95):
  blurbs say "In your saved copy, …" — change each to "On the Part N page
  or in your saved Posit copy, …" and change their `href` from
  `WORKSPACE_URL` to `"part2.html"` … `"part5.html"`, `cta` to
  `"Open Part N"`, remove `external: true`. (part-1 already points at the
  browser.)
- Bump `?v=` (css → 27, js → 7) in all `docs/*.html`.

**Verify**: `node --check docs/site.js` → exit 0; on the served homepage
with parts 1 marked complete, the next-step card CTA reads "Open Part 2"
→ part2.html.

### Step 7: Full link/anchor check

```bash
python3 - <<'EOF'
import re, os
docs = 'docs'
ids, links = {}, []
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
    page = (page or src).split('?')[0]
    if not page.endswith('.html'): continue
    if page not in ids: bad.append((src, href, 'missing page')); continue
    if frag and frag not in ids[page]: bad.append((src, href, 'missing anchor'))
print('OK' if not bad else bad)
EOF
```

**Verify**: prints `OK`, except JS-generated `#q-…` FAQ anchors, which are
acceptable.

## Test plan

Manual matrix on the served site, fresh profile: (1) homepage → warm-up →
finish → lands on Part 1 page; (2) homepage next-step card walks Parts 1–5
via part pages; (3) nav on every page shows the new order with "Posit
Cloud" in the support cluster; (4) submission page makes sense read as a
browser-only student AND as a Posit student; (5) FAQ filter "browser"
surfaces the new section; (6) JS disabled: all pages still read correctly
browser-first.

## Done criteria

- [ ] Posit-primacy grep (Commands table) → no matches
- [ ] Step verifications all pass; link checker `OK`
- [ ] All 12 navs identical (diff two of them after extracting the nav block)
- [ ] `?v=` consistent at one value per asset across all pages
- [ ] No commits made; `git status` shows working-tree changes only
- [ ] plans/README.md row updated

## STOP conditions

- The player files are absent from the working tree (drift check).
- You find yourself wanting to rebuild index.html beyond moving the
  posit-entry section — that is plan 008's scope.
- Any change would alter `assignments/*.Rmd`, `student-bundle/`, or
  `.github/` — out of scope, report instead.

## Maintenance notes

- Plan 008 (homepage) assumes this plan's framing landed; run 007 first.
- The "Dan sign-off" items ride with this plan: browser-first official +
  warm-up optional. Surface both explicitly when the teaching team reviews.
- `getting-started.html` keeps its filename; if it is ever renamed, sweep
  inbound links (nav ×12, FAQ, warm-up finish, part pages, submission).
