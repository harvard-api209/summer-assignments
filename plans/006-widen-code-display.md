# Plan 006: Give code a larger share of the screen on the warm-up page

> **Executor instructions**: Follow this plan step by step. Run every
> verification command and confirm the expected result before moving to
> the next step. If anything in the "STOP conditions" section occurs,
> stop and report. When done, update the status row in `plans/README.md`.
>
> **Drift check (run first)**: `git diff --stat e2dfc1c..HEAD -- docs/site.css docs/interactive-hour.html`
> On mismatch with the "Current state" excerpts, STOP and report.

## Status

- **Priority**: P3
- **Effort**: S
- **Risk**: LOW (CSS-only; worst case is a layout regression on mobile)
- **Depends on**: none (coordinate with 001/003 if running concurrently —
  same files)
- **Category**: dx (student-facing ergonomics)
- **Planned at**: commit `e2dfc1c`, 2026-06-11

## Why this matters

Faculty feedback: *"Is there a way to have a larger fraction of the
screen dedicated to where the code is?"* On the warm-up page
(`docs/interactive-hour.html`), each activity pairs a dark code card with
a quiz card in a two-column grid where the **code column is the smaller
one** (0.95fr vs 1.05fr, i.e. ~47% of the row) and code renders at
0.98rem. Code is the object of study on this page; it should dominate.

**Interpretation note**: this plan assumes the feedback refers to the
warm-up page (the only place the website displays code). If the professor
meant the knitted assignment HTML or the RStudio layout in Posit Cloud,
those are different changes — see Maintenance notes — and the advisor has
flagged the ambiguity to the team.

## Current state

`docs/site.css` (no build step; alphabetized properties; cache busted via
`?v=` on every HTML page — bump when changing):

```css
/* site.css:944-949 */
.activity-grid {
  display: grid;
  gap: 28px;
  grid-template-columns: minmax(0, 0.95fr) minmax(320px, 1.05fr);
  margin-top: 28px;
}

/* site.css:951-956 */
.pipe-example {
  display: grid;
  gap: 28px;
  grid-template-columns: minmax(0, 0.95fr) minmax(320px, 1.05fr);
  margin-top: 28px;
}

/* site.css:980-987 (inside .code-card code) */
  font-size: 0.98rem;
  line-height: 1.65;
```

In every `.activity-grid` / `.pipe-example` on
`docs/interactive-hour.html`, the code/data card is the FIRST child and
the quiz/reflection card is the second — except Warm-up 7 ("ai-check"),
whose first child is a `choice-group wide-card` (no code at all). The
mobile breakpoint (`@media (max-width: 900px)`) already collapses both
grids to one column — leave it alone.

## Commands you will need

| Purpose | Command | Expected on success |
|---------|---------|---------------------|
| Serve locally | `python3 -m http.server 8210 --directory docs` | site at :8210 |

## Scope

**In scope**: `docs/site.css`; `?v=` bump in all `docs/*.html`.

**Out of scope**: `docs/interactive-hour.html` markup; the assignment
`.Rmd` files; the mobile media queries' one-column layouts.

## Git workflow

- Branch off `2026-refresh`: `advisor/006-widen-code-display`
- Do NOT push or merge.

## Steps

### Step 1: Rebalance the grids toward code

In both `.activity-grid` and `.pipe-example`, change:

```css
grid-template-columns: minmax(0, 1.25fr) minmax(280px, 0.75fr);
```

Code now gets ~62% of the row; the quiz card keeps a 280px floor so
buttons never crush.

**Verify**: served site, Warm-up 2 (the 5-line objects example): the dark
card is visibly wider than the quiz card; no horizontal scrollbar inside
the code at 1280px viewport width.

### Step 2: Enlarge the code type

In `.code-card code`, change `font-size: 0.98rem` → `1.06rem`. Check the
longest line on the page (Warm-up 5's
`filter(district == "Boston") |>` block) does not wrap or overflow — the
card has `overflow-x: auto`, so worst case is a scrollbar, which is
acceptable but report it.

**Verify**: visual check at 1280px and at 901px (just above the mobile
breakpoint): no wrapped code lines in warm-ups 1–6.

### Step 3: Check the no-code section and mobile

Warm-up 7's grid has no code card; with the new ratios its `wide-card`
choice group takes the 1.25fr column — confirm it still reads fine. Then
at ≤900px confirm both grids stack to one column unchanged.

Bump `?v=` for `site.css` in all `docs/*.html`.

**Verify**: `grep -rho 'site\.css?v=[0-9]*' docs/*.html | sort -u` → one value.

## Test plan

Manual visual matrix: 1280px, 901px, 375px widths × warm-ups 1, 2, 5, 7.
Pass = code column visibly dominant on desktop, no overflow, mobile
unchanged.

## Done criteria

- [ ] Both grid rules use `minmax(0, 1.25fr) minmax(280px, 0.75fr)`
- [ ] `.code-card code` at `1.06rem`; no wrapped lines in warm-ups 1–6 at 1280px
- [ ] Mobile (≤900px) layout unchanged
- [ ] `git status` shows only `docs/site.css` + `?v=` bumps modified
- [ ] `plans/README.md` status row updated

## STOP conditions

- Plan 003's webR prototype has added run buttons/output areas to the
  code cards — re-evaluate the ratio with that extra content present
  rather than applying this blindly.
- The new ratio forces horizontal scrolling in any warm-up at 1280px
  after Step 2 — stop at `1.15fr/0.85fr` instead and note it.

## Maintenance notes

- If the professor actually meant the **knitted assignment HTML**: the
  fix would be a small CSS override in each `.Rmd` YAML (html_document
  default container is ~900px). That is an assignments change (plan 004
  territory), deliberately not done here — confirm intent first.
- If they meant **RStudio in Posit Cloud**: not controllable from this
  repo; the answer is a one-line tip in getting-started.html about
  dragging the pane divider. Cheap follow-up if confirmed.
