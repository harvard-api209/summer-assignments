# Plan 004: Apply faculty content revisions to the assignment notebooks

> **Executor instructions**: Follow this plan step by step. Run every
> verification command and confirm the expected result before moving to
> the next step. If anything in the "STOP conditions" section occurs,
> stop and report — do not improvise. When done, update the status row in
> `plans/README.md`.
>
> **Drift check (run first)**: `git diff --stat e2dfc1c..HEAD -- assignments/`
> On mismatch with the "Current state" excerpts, STOP and report.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW (text edits to teaching materials; the risk is breaking a
  knit, which the verification step catches)
- **Depends on**: none
- **Category**: docs (course content)
- **Planned at**: commit `e2dfc1c`, 2026-06-11

## Why this matters

The course professor reviewed the five R Markdown assignments and asked
for specific changes: cut a low-value question in Part 2, fix wording and
re-frame the exercises in Part 3 (students should adapt provided code,
not memorize ggplot syntax), and clean up the "Before you submit"
checklists. The checklists currently differ across the five files in
arbitrary ways — e.g. only Part 1 mentions reading the first error
message, no file reminds students to verify they are in their own saved
Posit Cloud copy (the single most damaging mistake: working in a
temporary copy loses work), and only Parts 3/5 use the shorter "Knit
successfully". This plan applies the professor's edits exactly and
standardizes the checklists into a shared core plus per-part checks.

## Current state

Five hand-written R Markdown files in `assignments/`. Each has YAML
front matter (`author: "YOUR NAME"`), an intro, teaching sections with
worked code, exercise sections with empty chunks (`# Write your code
here.`), an "AI-use note" section, and a closing "## Before you submit"
bullet list. Prose is wrapped at 80 columns (`editor_options: markdown:
wrap: 80`) — match that when editing.

Exact excerpts this plan edits:

**`assignments/R Summer Assignment 2.Rmd`** — lines 213–217 (item 3 of
"## Exercises 2: ranking", which lines 196–212 precede with two sorting
exercises):

```
213:
214:3. Write one sentence comparing what the two rankings are measuring. Do not make
215:a causal claim.
216:
217:Write your answer here:
```

Lines 287–293 (checklist):

```
287:## Before you submit
288:
289:- Add your name at the top.
290:- Run your code chunks.
291:- Knit the document successfully.
292:- Make sure each filter matches the written question.
293:- Submit the completed `.Rmd` file to Canvas.
```

**`assignments/R Summer Assignment 3.Rmd`** — headings at lines 80, 122,
198: `## Exercises 1: trends`, `## Exercises 2: coherent peer comparison`,
`## Exercises 3: associations`. Lines 94–95:

```
94:3. In one sentence, describe the trend. Is this a trend, comparison, or
95:association?
```

Intro paragraph ends at line 25 with: `Do not claim that one variable
causes another.` Checklist at lines 253–259 (5 bullets: name / labels /
no causal claims / "Knit successfully." / submit).

**`assignments/R Summer Assignment 2.Rmd` and `4.Rmd`** also use the
plural heading style: A2 lines 137/196/256, A4 lines 81/159/191.

**Checklists in the other files** (verify with
`for f in assignments/*.Rmd; do awk '/^## Before you submit/,0' "$f"; done`):
A1 (5 bullets incl. "If knitting fails, read the first error message and
fix that chunk first."), A4 (5 bullets incl. row-meaning and denominator
checks), A5 (8 bullets incl. peers/missingness/labels/causal/memo).

## Commands you will need

| Purpose | Command | Expected on success |
|---------|---------|---------------------|
| Heading sweep | `grep -n "^## Exercises" assignments/*.Rmd` | see steps |
| Checklist dump | `for f in assignments/*.Rmd; do echo "== $f"; awk '/^## Before you submit/,0' "$f"; done` | see steps |
| Optional knit check | `Rscript -e 'for (f in list.files("assignments", full.names=TRUE, pattern="[.]Rmd$")) rmarkdown::render(f, quiet=TRUE)'` | exit 0; HTML files appear next to the Rmds |

The knit check requires a local R installation with `rmarkdown` and
`tidyverse`. If `Rscript` is unavailable, skip it and say so in your
report — the edits in this plan are prose-only and cannot break code, but
the knit check is the gold-standard gate. **Never commit the rendered
`.html` files** (the repo intentionally stopped tracking them).

## Scope

**In scope**:
- `assignments/R Summer Assignment 1.Rmd` (checklist only)
- `assignments/R Summer Assignment 2.Rmd`
- `assignments/R Summer Assignment 3.Rmd`
- `assignments/R Summer Assignment 4.Rmd` (headings + checklist)
- `assignments/R Summer Assignment 5.Rmd` (checklist only)

**Out of scope** (do NOT touch):
- Any R code chunk contents.
- `docs/` (website — other plans own it).
- Assignment 5's release timing (plan 005).
- The "Estimated time" lines, YAML headers, and section order.

## Git workflow

- Branch off `2026-refresh`: `advisor/004-assignment-revisions`
- Short imperative commit subjects (e.g. "Revise assignment checklists and
  Part 3 exercises"). Do NOT push or merge.

## Steps

### Step 1: Cut the rankings-comparison question in Part 2

In `R Summer Assignment 2.Rmd`, delete lines 213–217 (the blank line,
item 3, and its "Write your answer here:" prompt). "## Exercises 2:
ranking" then contains exactly items 1 and 2 — do not renumber anything.

**Verify**: `grep -n "two rankings" assignments/*.Rmd` → no matches.
`grep -c "Write your answer here" "assignments/R Summer Assignment 2.Rmd"` → 4 (was 5).

### Step 2: Singularize the exercise headings

In A2, A3, and A4, change every `## Exercises N:` heading to
`## Exercise N:` (9 headings total). The professor's request named
Part 3; applying it to A2/A4 keeps the three notebooks consistent — if
the teaching team wants Part 3 only, they will say so at review (note it
in your report).

**Verify**: `grep -rn "^## Exercises" assignments/` → no matches;
`grep -rc "^## Exercise [0-9]" assignments/*.Rmd` → A2: 3, A3: 3, A4: 3.

### Step 3: Re-frame Part 3 exercises around adapting provided code

In `R Summer Assignment 3.Rmd`:

1. After the intro paragraph (line 25, "…Do not claim that one variable
   causes another."), insert a blank line and this paragraph (wrap at 80):

   ```
   A note on the exercises: copy the example code just above each exercise and
   adapt it -- change the country, the variable, the labels. Adapting working
   code is how analysts actually work. The goal is to read and adjust ggplot
   code with judgment, not to memorize its syntax.
   ```

2. In "## Exercise 1: trends", item 2 ("Make a line plot of that
   country's under-5 mortality over time."), append to the same item:
   `Start from the Ghana example above and change the country object and the y-axis variable.`

3. Replace lines 94–95 so item 3 reads exactly:

   ```
   3. In one sentence, describe the trend.
   ```

**Verify**: `grep -n "trend, comparison, or" "assignments/R Summer Assignment 3.Rmd"`
→ no matches. `grep -n "memorize" "assignments/R Summer Assignment 3.Rmd"` → 1 match.

### Step 4: Standardize the "Before you submit" checklists

Replace each file's `## Before you submit` section with the shared core
(6 items) plus the per-part items shown below, inserted as item 5 (after
"Complete the AI-use note…"). The core, exactly:

```
## Before you submit

- Add your name in the YAML header at the top, replacing `YOUR NAME`.
- Confirm you are in your own saved Posit Cloud copy, not a temporary copy.
- Run your code chunks, then knit the document successfully. If knitting
  fails, read the first error message and fix that chunk first.
- Complete the AI-use note.
<PER-PART ITEMS HERE>
- Submit the completed `.Rmd` file to Canvas.
```

Per-part items:

- **A1**: (none — the core covers it)
- **A2**:
  - `- Make sure each filter matches the written question.`
  - `- Check missing values before interpreting poverty or inequality.`
- **A3**:
  - `- Make sure your plots have meaningful labels.`
  - `- Do not make causal claims from descriptive graphs.`
- **A4**:
  - `- Check what one row represents before and after each summary.`
  - `- Include denominators when summarizing sparse indicators.`
- **A5**: replace the core's AI-use line with
  `- Complete the memo and the AI-use reflection.` and add:
  - `- Check that your peer comparison is coherent.`
  - `- Check missing values for your main indicator.`
  - `- Make sure your graphs have meaningful labels and avoid causal claims.`

**Verify**: the checklist dump command shows: every file starts with the
same first four bullets; A1 has 5 bullets total, A2/A3/A4 have 7, A5
has 8. `grep -c "own saved Posit Cloud copy" assignments/*.Rmd | grep -v ':1'`
→ no output (every file has exactly one).

### Step 5: Knit check (if R is available)

Run the optional knit command from the table. All five render without
error. Delete the generated `.html` files afterwards
(`rm assignments/*.html`) — do not commit them.

**Verify**: command exits 0, `git status` shows no `.html` files.

## Test plan

No automated tests exist for course content. The gates are: the greps in
each step, the knit check (Step 5) when R is available, and a human read
of `git diff` confirming no code chunk changed
(`git diff -- assignments/ | grep '^[-+]' | grep -v '^[-+ ]*[-#*<>]' `
should show only prose lines).

## Done criteria

- [ ] All step verifications pass
- [ ] `git diff --stat` touches only the five `.Rmd` files
- [ ] No `.html` files staged or committed
- [ ] Knit check passed, or explicitly reported as skipped with the reason
- [ ] `plans/README.md` status row updated

## STOP conditions

- Line numbers in "Current state" don't match the files (drift) — re-locate
  by searching the quoted text; if the quoted text itself is gone, STOP.
- Any edit would change the contents of an R code chunk.
- The knit check fails on a file this plan did NOT edit — pre-existing
  breakage; report it rather than fixing it here.

## Maintenance notes

- The "own saved Posit Cloud copy" checklist line duplicates guidance on
  the website (submission.html). If the platform changes (see plan 002's
  maintenance notes), update both.
- Decision recorded for the teaching team: the professor's pasted
  checklist mixed Part-2-specific items into a generic list; this plan
  keeps a standard core + per-part specifics instead. If the team truly
  wants one identical list in all five files, it is a 10-minute follow-up.
- If plan 005 ends with Part 5 released later, its checklist still ships
  with the file — no interaction with this plan.
