# Implementation Plans

Maintained by the improve skill. Two generations:

- **Gen 1** (2026-06-11, commit `e2dfc1c`): plans 001–006 — counting
  unification, Posit decoupling, webR spike, notebook revisions, Part 5
  gate, code display. All DONE except 005 (decision-gated).
- **Gen 2** (2026-06-12, commit `5be5e92` + **uncommitted working tree**):
  plans 007–013 — the browser-first pivot (grilled with the teaching team
  and approved) plus vetted findings from a 36-agent audit of the new
  in-browser assignment player.

**Ground rules for Gen 2 executors** (historical — Gen 2 shipped on
2026-06-12 with the teaching team's go-ahead):
1. (superseded) The plans were executed in the working tree and pushed
   live after verification (`bash tools/verify.sh`).
2. Three decisions need Dan's sign-off before any of this goes live:
   browser-first as official policy (007), warm-up demoted to optional
   (007), AI manifesto compression (008). Build now; flag at review.
3. The Posit Cloud side is untouchable: `assignments/*.Rmd`,
   `student-bundle/`, `.github/workflows/student-branch.yml`.
4. After editing `docs/site.css` or `docs/site.js`, bump their `?v=`
   query strings in ALL `docs/*.html`.
5. Local preview: `python3 -m http.server 8210 --directory docs`.

## Execution order & status

| Plan | Title | Priority | Effort | Depends on | Status |
|------|-------|----------|--------|------------|--------|
| 001 | Unify the site on one counting model | P1 | M | — | DONE (2026-06-11) |
| 004 | Faculty content revisions to the notebooks | P1 | M | — | DONE (2026-06-11, knit-verified) |
| 005 | Part 5 release strategy | P2 | S | teaching-team decision | BLOCKED (awaiting date) |
| 002 | Decouple the site from Posit Cloud | P2 | M | 001 | DONE (2026-06-11) |
| 006 | Larger code share on the warm-up page | P3 | S | — | DONE (2026-06-11) |
| 003 | Spike: in-browser R (webR) | P3 | M | — | DONE (adopted; grew into the full player) |
| **007** | **Browser-first as the official default (IA sweep)** | P1 | M | — | DONE (2026-06-12) |
| **008** | **Homepage rebuilt around the roadmap** | P1 | M | 007 | DONE (2026-06-12; setup/warm-up also removed from tracked STEPS for browser-first coherence) |
| **009** | **Auto "in progress" detection; completion stays manual** | P1 | M | 008 | DONE (2026-06-12) |
| **010** | **Pre-warm R on first interaction + honest engine states** | P1 | M | — | DONE (2026-06-12; implemented inside webr-runner.js) |
| **011** | **Player hardening: truthful autosave, pinned webR (v0.5.4), sanitized export, validated parse, shared runner** | P2 | M | 010 | DONE (2026-06-12) |
| **012** | **Commit the web-assignment generator (anti-drift)** | P2 | M | — | DONE (2026-06-12; in tools/ not scripts/ — scripts/ is copied to the student branch by the Action) |
| **013** | **One-command verification baseline (round-trip tests + link checker)** | P3 | M | after 010/011 settle | DONE (2026-06-12; bash tools/verify.sh, 15 tests) |

Status values: TODO | IN PROGRESS | DONE | BLOCKED (reason) | REJECTED (rationale).

## Dependency notes

- **007 → 008 → 009** is a strict chain (framing → structure → live state).
- **010 → 011** share `docs/assignment-player.js`; 011 also expects 010's
  engine-state contract. 012/013 are independent of the IA chain; 013 last.
- 005 remains gated on the Part 5 release date; unchanged by Gen 2.

## Open items for the teaching team

- Dan sign-offs listed in ground rule 2.
- Part 5 date (plan 005); Slack link; assignment deadlines (pre-existing).
- Ship decision: when approved, wire `scripts/verify.sh` (013) and the
  generator `--check` (012) into CI, and commit/push everything.

## Findings considered and rejected (Gen 2 audit — do not re-audit)

- **Shelter leak on tab close mid-run**: the browser frees the whole wasm
  heap on unload; the run-button disable already prevents concurrent runs.
  No residual harm.
- **CSV refetched every page load**: GitHub Pages serves ETags; the HTTP
  cache already handles repeat loads. IndexedDB caching is complexity
  without measurable win.
- **"DevTools lets students run arbitrary R"**: their own sandboxed wasm
  in their own tab — no victim, no trust boundary crossed.
- **`rel=noopener` on the same-tab zip link**: cosmetic; no opener handed
  out on same-tab navigation.
- **Markup duplication across HTML files / cache-busting discipline**
  (refuted by verifier as overstated): cost of a static no-build site;
  revisit only if page count grows.
- **Hub lacks a visual preview of the player** (MED): real but minor;
  noted as optional polish inside 008's how-it-works strip rather than a
  plan.

Gen 1 rejected items (kept for the record): nav/footer de-duplication via
JS injection; "7 steps everywhere" counting; platform-neutral renaming of
Posit Cloud prose.
