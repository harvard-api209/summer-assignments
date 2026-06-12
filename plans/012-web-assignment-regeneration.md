# Plan 012: Commit the web-assignment generator; stop the generated files from drifting

> **Executor instructions**: Follow steps + verifications; STOP conditions
> binding; update `plans/README.md` when done.
>
> **CRITICAL — working-tree state**: `docs/web-assignments/` is
> UNCOMMITTED on `2026-refresh`. Work in the existing working tree. NO
> commits/pushes (the generator script you write is also left uncommitted
> for review).
>
> **Drift check (run first)**: `ls docs/web-assignments/part-*.Rmd` → five
> files; `ls scripts/` contains only `refresh_development_indicators.R`.
> If a generator already exists in scripts/, STOP and reconcile.

## Status

- **Priority**: P2
- **Effort**: M
- **Risk**: LOW (a new script + docs; site behavior unchanged if output
  is byte-identical — which is the acceptance test)
- **Depends on**: none (but run BEFORE any future content edit to
  assignments/*.Rmd)
- **Category**: tech-debt (vetted audit finding: generated files have no
  in-repo generator)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12

## Why this matters

`docs/web-assignments/part-1.Rmd`…`part-5.Rmd` are browser-adapted copies
of the canonical `assignments/R Summer Assignment N.Rmd` files (Posit
references stripped, knitting content removed, `../data/` → `data/`,
web-specific submit checklist). They were produced by a one-off script
that lives only in a chat transcript. The next time the teaching team
edits an assignment, the web copy silently drifts — the exact two-sources
failure this repo already suffered once between branches. The fix: a
committed, idempotent generator plus documentation, wired into the
maintenance docs so "edit assignment → regenerate web copy" is one
command.

## Current state

- Canonical sources: `assignments/R Summer Assignment {1..5}.Rmd`.
- Generated: `docs/web-assignments/part-{1..5}.Rmd` (+ `data/*.csv`
  copies of `data/*.csv`).
- The transformation rules that produced today's files (re-derive by
  diffing source vs generated — `diff <(cat "assignments/R Summer
  Assignment 2.Rmd") docs/web-assignments/part-2.Rmd` shows them all):
  1. Remove sections "## Before you start*" and "## Why knit?" (heading
     through the line before the next `^## ` heading).
  2. Remove "### Knit checkpoint" sections (through next `^#{2,3} `).
  3. Replace `../data/` with `data/`.
  4. Replace the entire "## Before you submit" section with the
     web-specific checklist (browser wording: name in panel, run top to
     bottom and fix errors/warnings before downloading, AI note,
     part-specific items, download button + Canvas). The exact current
     text per part IS the spec — extract it from today's generated files.
  5. A small list of per-part prose substitutions (knit phrasing in "How
     to work through this file", Part 1's bridge sentence, Part 1's
     learning-goals knitting bullet, Part 1's eval=FALSE explanation).
     Again: derive the exact pairs from the diffs.
- `BRANCHING.md` documents repo branch flow; `.github/workflows/
  student-branch.yml` regenerates the student branch — read both for
  style/integration points but DO NOT modify the workflow in this plan
  (the teaching team hasn't shipped the web feature yet; wiring CI comes
  with the ship decision).

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Regenerate | `python3 scripts/generate_web_assignments.py` | exit 0, writes 5 files |
| Idempotence check | `git diff --stat docs/web-assignments/` after running on today's inputs | (see Step 2 note — files are untracked; use the /tmp snapshot diff instead) |
| Leftover scan | `grep -n "Posit\|[Kk]nit\|\.\./data" docs/web-assignments/part-*.Rmd \| grep -v "knitr::opts_chunk"` | no output |

## Scope

**In scope**: NEW `scripts/generate_web_assignments.py`; NEW
`docs/DEVELOPMENT.md`; a note in `BRANCHING.md`'s edit-flow section.
**Out of scope**: `.github/workflows/*` (note the future wiring in
DEVELOPMENT.md instead); any change to the generated CONTENT (the
acceptance test is byte-identical output); `assignments/*.Rmd`.

## Steps

### Step 1: Write the generator

`scripts/generate_web_assignments.py`, stdlib-only, deterministic:
reads `assignments/R Summer Assignment {N}.Rmd`, applies rules 1–5,
writes `docs/web-assignments/part-{N}.Rmd`, and copies `data/*.csv` to
`docs/web-assignments/data/`. Encode rules 4–5 as explicit per-part
constants WITH `assert old in text` guards so content drift fails loudly
instead of silently skipping. End by printing the per-part answer-box
counts (`^\[Write your` matches) — plan 009's PART_ANSWER_TOTALS reminder.
Include a `--check` mode: regenerate to a temp dir and exit non-zero with
a diff summary if the committed outputs differ (future CI hook).

### Step 2: Prove byte-identical output

Snapshot today's generated files (`cp -r docs/web-assignments /tmp/wa-before`),
run the generator, then `diff -r /tmp/wa-before docs/web-assignments`.

**Verify**: diff is empty. If it is NOT empty, your re-derived rules are
incomplete — fix the script until empty; NEVER adjust the generated files
to match the script.

### Step 3: DEVELOPMENT.md

`docs/DEVELOPMENT.md` (~1 page): the site has no build; the player
architecture in five sentences (parseRmd → render → webR run → autosave
keys `api209-part{N}-work-v1` → buildRmd export must round-trip);
the generated-files rule (NEVER hand-edit `docs/web-assignments/*.Rmd`;
edit `assignments/*.Rmd` then run the generator); cache-busting `?v=`
discipline; how to preview (`python3 -m http.server 8210 --directory docs`);
pointer to `plans/` and BRANCHING.md. Add one line to BRANCHING.md's
"How notebook edits flow": after editing assignments/, ALSO run
`python3 scripts/generate_web_assignments.py` (until CI does it).

### Step 4: Leftover scan + answer-count cross-check

Run the leftover scan (Commands table) → empty. Cross-check printed
answer counts against `docs/site.js` PART_ANSWER_TOTALS if plan 009 has
landed (else note the counts in your report for 009's executor).

## Test plan

Step 2 IS the test (byte-identical regeneration), plus: corrupt one
assertion input deliberately (edit a copy of an assignment in /tmp, point
the script at it via a temp arg or env) → script exits non-zero with a
clear message naming the missing text. `--check` mode returns 0 on
pristine outputs, non-zero after you touch one generated file (restore it
afterwards from /tmp snapshot).

## Done criteria

- [ ] `python3 scripts/generate_web_assignments.py` reproduces today's
      five files byte-identically (empty diff vs /tmp snapshot)
- [ ] `--check` mode works both directions
- [ ] Leftover scan empty; answer counts printed
- [ ] DEVELOPMENT.md exists; BRANCHING.md edit-flow updated
- [ ] No commits; plans/README.md updated

## STOP conditions

- Byte-identical proves impossible because the generated files contain a
  manual hand-edit not derivable from the sources — STOP and report the
  exact hunk; the teaching team must decide whether the hand-edit becomes
  a rule or is dropped.
- You are tempted to "improve" the generated prose while in there — the
  acceptance test forbids it by construction; content changes belong to
  the teaching team.

## Maintenance notes

- When the teaching team approves shipping the browser feature, add the
  generator's `--check` to `.github/workflows/student-branch.yml` paths
  (or a small verify workflow — plan 013) so CI fails on drift.
- New assignment parts: add a per-part constants entry; the asserts will
  guide what is missing.
