# Plan 005: Decide and implement the Part 5 release strategy (listed-but-locked)

> **Executor instructions**: This plan has a DECISION GATE. Steps 1–2
> prepare the decision; do not execute Step 3+ until the teaching team
> has chosen an option and a date. If you reach the gate without a
> decision, stop, update the index row to BLOCKED with "awaiting
> teaching-team decision", and report.
>
> **Drift check (run first)**: `git diff --stat e2dfc1c..HEAD -- docs/index.html docs/site.js`
> On mismatch with the "Current state" excerpts, STOP and report.

## Status

- **Priority**: P2
- **Effort**: S (site work) — the decision itself is the hard part
- **Risk**: LOW
- **Depends on**: plans/001-unify-part-counting.md (the roadmap markup and
  "5 parts" vocabulary this plan annotates)
- **Category**: direction (course design)
- **Planned at**: commit `e2dfc1c`, 2026-06-11

## Why this matters

The professor asked: *"Can we review assignment 5 and decide whether we
keep as is or do what we discussed the other day (release it later)?"*
Part 5 (Mini Development Diagnostic Memo, estimated 3–4 hours) is the
capstone: it depends on skills from Parts 1–4 and is the piece the
teaching team is most likely to want to revise after seeing how students
handle Parts 1–4. Releasing it later (a) lets the team revise it with
real information, and (b) shortens the apparent workload wall on day one.
The risk is re-creating the exact confusion plan 001 fixes: if Part 5
silently disappears from the site, the "5 parts" count stops matching
what students can see. The recommendation is therefore **listed but
locked**: Part 5 stays on the roadmap and in every count, visibly marked
"releases on [date]".

## Current state

- `docs/index.html` — the roadmap (`section id="assignments"`) lists five
  `<article class="assignment">` blocks; Part 5 is `id="part-5"`
  ("Mini Development Diagnostic Memo … Estimated time: 3-4 hours.").
  After plan 001, each article also carries a JS-injected "Mark this part
  complete" checkbox.
- `docs/site.js` — `STEPS` contains a `part-5` entry whose blurb starts
  "In your saved copy, choose one country and coherent peers…". The
  next-step card surfaces it when parts 1–4 are done.
- `assignments/R Summer Assignment 5.Rmd` — the file itself, present in
  this repo AND copied into the Posit Cloud project (the Posit project is
  maintained by hand by the teaching team; it is NOT auto-synced from
  this repo).
- The submission page (`docs/submission.html`) lists Assignment 5 with
  "Canvas link forthcoming" — already consistent with a later release.

## Commands you will need

| Purpose | Command | Expected on success |
|---------|---------|---------------------|
| Serve locally | `python3 -m http.server 8210 --directory docs` | site at :8210 |
| Count check | `grep -c "assignment-kicker" docs/index.html` | 5 (before and after) |

## Scope

**In scope** (only after the decision gate):
- `docs/index.html` (Part 5 roadmap article)
- `docs/site.js` (Part 5 STEPS blurb; release-tag styling hook if needed)
- `docs/site.css` (a `.release-tag` style)

**Out of scope**:
- Deleting `assignments/R Summer Assignment 5.Rmd` from this repo — the
  file stays; only its presence in the *Posit Cloud project* changes, and
  that is a manual teaching-team operation outside this repo.
- The Canvas link rows in `docs/submission.html` (already "forthcoming").
- Any change that removes Part 5 from counts, the roadmap, or the
  progress denominator.

## Git workflow

- Branch off `2026-refresh`: `advisor/005-part5-staged-release`
- Do NOT push or merge.

## Steps

### Step 1: Present the decision memo (no code)

Put this in front of the teaching team:

**Option A — keep as is.** Part 5 ships with the project from day one.
Pro: zero work, ambitious students can plan ahead, no second release
operation in August. Con: the team cannot revise Part 5 based on Parts
1–4 experience without students having seen the old version; the day-one
workload looks larger.

**Option B — listed but locked (recommended).** Part 5 stays on the
roadmap and in all counts with a visible "Releases on DATE" tag; the
`.Rmd` is removed from the Posit Cloud project until that date. Pro:
revisable, gentler day one, count stays honest. Con: one manual release
operation (add the file back to the Posit project + remove the site tag),
which MUST be calendared or students hit a missing file.

Required outputs of the gate: **A or B**, and if B, the **release date**.

### Step 2: DECISION GATE

If Option A → mark this plan DONE with note "decision: keep as is", stop.
If no decision → mark BLOCKED, stop. If Option B with a date → continue.

### Step 3 (B only): Tag Part 5 on the roadmap

In `docs/index.html`, inside the `id="part-5"` article, after the
`assignment-kicker` div, add:

```html
<p class="release-tag">Releases on <strong>DATE</strong> — the file appears
in your Posit Cloud copy then. Parts 1–4 are available now.</p>
```

In `docs/site.css` (2026-refresh section, alphabetized properties):

```css
.release-tag {
  color: var(--accent);
  font-size: 0.88rem;
  font-weight: 600;
  margin: 0 0 8px;
}
```

In `docs/site.js`, change the `part-5` STEPS blurb to mention the date,
e.g. "Releases on DATE. Choose one country and coherent peers, produce
the required outputs, and write the short non-causal memo." Bump the
`?v=` cache busters on `site.css`/`site.js` in all `docs/*.html`.

**Verify**: served site shows the tag on Part 5; marking parts 1–4
complete makes the next-step card show Part 5 *with* the release-date
blurb; `grep -c "assignment-kicker" docs/index.html` → still 5.

### Step 4 (B only): Write the release-day runbook

Append to this plan file (or hand to the team) the checklist:

1. Add `R Summer Assignment 5.Rmd` back to the Posit Cloud project
   (upload into `assignments/`; verify it knits there).
2. Remove the `.release-tag` paragraph from `docs/index.html`; restore
   the original `part-5` blurb in `docs/site.js`; bump `?v=`.
3. Post the Canvas link for Assignment 5 (submission.html row).
4. Announce in the course channel.

Calendar it for the chosen date with an owner.

## Test plan

Manual: the verification in Step 3, plus a JS-disabled load (the static
tag must read correctly without JS).

## Done criteria

- [ ] Decision recorded in `plans/README.md` (A: DONE+note / blocked / B)
- [ ] (B) Tag visible on roadmap and in next-step card; counts unchanged
      (still 5 parts everywhere)
- [ ] (B) Release-day runbook exists with a named owner and date
- [ ] `git status` shows only in-scope files modified

## STOP conditions

- The decision gate (Step 2) — by design.
- Plan 001 has not landed (this plan edits markup 001 restructures).
- Anyone proposes removing Part 5 from the roadmap or the 5-part count —
  that re-creates the "5 or 7 parts?" confusion; escalate instead.

## Maintenance notes

- The release-day runbook is the fragile piece: the site tag and the
  Posit project must change on the same day. A stale "Releases on DATE"
  after the date is worse than either option alone.
- Next summer: revisit whether Part 5 should simply ship day one if this
  cohort's data shows few students reached it early anyway.
