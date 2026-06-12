# Developer notes — API-209 student site

Plain static site, no build step: hand-written HTML in `docs/`, vanilla JS,
served by GitHub Pages from `2026-refresh:/docs`. Preview locally with
`python3 -m http.server 8210 --directory docs`.

## Architecture in five sentences

`site.js` runs on every page: nav highlighting, the homepage dashboard
(progress in localStorage key `api209-course-progress-v1`), FAQ
filter/anchors. Each assignment page (`part1.html`–`part5.html`) sets
`window.API209_PART` and loads `webr-runner.js` (shared, **pinned** webR +
engine state machine + data staging) and `assignment-player.js` (fetches
the part's Rmd from `web-assignments/`, renders it, autosaves to
`api209-part{N}-work-v1`, exports the completed `.Rmd`). The export
invariant: `buildRmdFrom(parseRmd(src), emptyState)` reproduces `src`
byte-for-byte, so browser submissions match Posit Cloud submissions.
R itself runs client-side via webR; Parts 2–5 pre-install tidyverse in the
background on the student's first keystroke. The warm-up
(`interactive-hour.html`) keeps its own quiz logic but shares the runner.

## Rules

- **Never hand-edit `docs/web-assignments/*.Rmd`.** Edit the canonical
  `assignments/R Summer Assignment N.Rmd`, then run
  `python3 tools/generate_web_assignments.py`. The script asserts on
  drift and prints the answer-box counts — keep `PART_ANSWER_TOTALS` in
  `docs/site.js` in sync with them.
- **Bump cache busters** (`site.css?v=`, `site.js?v=`,
  `assignment-player.js?v=`, `webr-runner.js?v=`) in every HTML page that
  references a file you changed.
- **webR version** is pinned in one place (`WEBR_URL` in
  `docs/webr-runner.js`). To bump it, change the constant and re-run the
  part-page test matrix in `plans/010` before pushing.
- **localStorage keys are API.** `api209-part{N}-work-v1` is written by
  the player and read by `site.js` for progress badges — change the schema
  on the writer and you must update the reader.

## Verification

```
bash tools/verify.sh
```

Runs: JS syntax checks, the player round-trip test suite
(`tests/player.test.mjs` — the export-integrity guarantee), the internal
link/anchor checker, and the generated-files drift check. Run it before
every push. CI wiring is a five-line GitHub Action step
(`on: push: paths: docs/**` → `bash tools/verify.sh`) — add it alongside
`.github/workflows/student-branch.yml` if pushes become frequent.

Branch model and season-end checklist: see `BRANCHING.md`. Plans and audit
history: see `plans/README.md`.
