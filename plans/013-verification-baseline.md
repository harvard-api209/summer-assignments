# Plan 013: A one-command verification baseline for the site

> **Executor instructions**: Follow steps + verifications; STOP conditions
> binding; update `plans/README.md` when done.
>
> **CRITICAL — working-tree state**: key files are UNCOMMITTED on
> `2026-refresh`. Work in the existing working tree. NO commits/pushes.
>
> **Drift check (run first)**: `docs/assignment-player.js` exists and
> contains functions `parseRmd` and `buildRmd` (grep). `node --version`
> ≥ 18 (the test runner uses node:test). Otherwise STOP.

## Status

- **Priority**: P3
- **Effort**: M
- **Risk**: LOW (additive: tests + scripts; no site behavior changes)
- **Depends on**: best run AFTER 010/011 settle assignment-player.js's
  final shape (tests written against a moving file churn)
- **Category**: tests (vetted audit finding: zero verification for the
  export pipeline that produces graded submissions)
- **Planned at**: commit `5be5e92` + uncommitted working tree, 2026-06-12

## Why this matters

~1,500 lines of hand-written JS now stand between a student's work and
their graded submission, and the only verification is eyeballs in a
preview browser. The single highest-value invariant is the **round-trip**:
`buildRmd(parseRmd(source))` with untouched state must equal the source,
and with edits must equal the source with exactly those substitutions —
this is what guarantees browser students submit the same document as
Posit students. Today nothing checks it; a one-line parser regression
would corrupt every export silently. This plan adds a no-build,
no-dependency verification: node unit tests for the parser/export, the
link/anchor checker as a committed script, and one command that runs
everything.

## Current state

- No package.json, no tests, no CI for the site (the only workflow is
  `.github/workflows/student-branch.yml`, unrelated).
- `docs/assignment-player.js` is an IIFE — `parseRmd`/`buildRmd` are not
  importable. The plan's enabling refactor: expose pure functions without
  changing behavior (see Step 1). If plan 011 created
  `docs/webr-runner.js`, the player file is already slimmer; the parse/
  export logic is still in assignment-player.js.
- The link/anchor checker exists only as a snippet inside
  `plans/007-browser-first-ia.md` (Step 7).
- Inputs for fixtures: `docs/web-assignments/part-{1..5}.Rmd`.

## Commands you will need

| Purpose | Command | Expected |
|---------|---------|----------|
| Tests | `node --test tests/` | all pass |
| Everything | `bash scripts/verify.sh` | `ALL CHECKS PASSED`, exit 0 |

## Scope

**In scope**: a minimal-export refactor inside `docs/assignment-player.js`
(Step 1 ONLY — no behavior change), NEW `tests/player.test.mjs`, NEW
`scripts/check_links.py` (lift from plan 007 Step 7), NEW
`scripts/verify.sh`, a "Verification" section in `docs/DEVELOPMENT.md`
(created by plan 012; create the file with just this section if 012 has
not run).

**Out of scope**: CI wiring (decision-coupled to shipping; document the
one-liner in DEVELOPMENT.md); browser-automation tests (preview tooling
is environment-specific); any change to parse/export semantics.

## Steps

### Step 1: Make the pure core testable (no behavior change)

In `docs/assignment-player.js`, the parsing/export functions close over
`CONFIG`/`state`. Refactor minimally so the pure logic is reachable from
node:

- Extract `parseRmd(text)` and a parameterized
  `buildRmdFrom(parsed, state, name)` (current `buildRmd()` becomes a
  thin wrapper passing the live state).
- At the END of the IIFE add a guarded export hook:

```js
if (typeof window !== "undefined") {
  window.api209PlayerInternals = { parseRmd: parseRmd, buildRmdFrom: buildRmdFrom };
} else if (typeof module !== "undefined") {
  module.exports = { parseRmd: parseRmd, buildRmdFrom: buildRmdFrom };
}
```

…BUT the file is an IIFE that early-returns when `#player-doc` is absent
(`if (!CONFIG || !root) return;`) — move the pure functions ABOVE that
guard (they reference nothing DOM-bound) so the node path reaches them.
Verify in a browser that part1 still boots and exports identically
(console round-trip check) before proceeding.

### Step 2: Round-trip unit tests

`tests/player.test.mjs` using `node:test` + `node:assert`, loading the
player via `createRequire`/`require("../docs/assignment-player.js")`
(CommonJS hook from Step 1):

1. **Identity round-trip ×5**: for each `docs/web-assignments/part-N.Rmd`,
   `buildRmdFrom(parseRmd(src), {chunks:{},answers:{}}, "")` equals `src`
   modulo a single trailing newline.
2. **Substitution**: edit chunk 0 + answer 0 on part-1 → output contains
   exactly those texts at the right positions; placeholder count drops by
   one; all other lines unchanged (diff line count == expected).
3. **Name handling**: name lands in YAML author; (post-011) newline/quote
   names are sanitized — if 011 hasn't run, assert current behavior and
   leave a TODO referencing 011.
4. **Structure counts**: parse of each part yields the expected number of
   editable chunks and answer boxes (snapshot today's numbers into the
   test with a comment pointing at plan 012's generator counts).
5. **Malformed input**: unbalanced fence input → (post-011) validateParse
   reports it; pre-011, document current behavior.

### Step 3: Committed link checker + verify.sh

`scripts/check_links.py`: the plan-007 snippet, plus `--strict` exit code
(non-zero on any bad link, JS-generated `#q-*` anchors allowlisted).
`scripts/verify.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
node --check docs/site.js
node --check docs/assignment-player.js
[ -f docs/webr-runner.js ] && node --check docs/webr-runner.js
node --test tests/
python3 scripts/check_links.py --strict
[ -f scripts/generate_web_assignments.py ] && python3 scripts/generate_web_assignments.py --check
echo "ALL CHECKS PASSED"
```

### Step 4: Document

Add "Verification" to `docs/DEVELOPMENT.md`: run `bash scripts/verify.sh`
before any push; what each check catches; the CI one-liner to add when
the feature ships (a workflow step running the same script on pushes
touching `docs/**`).

## Test plan

`bash scripts/verify.sh` → ALL CHECKS PASSED. Then prove the tests bite:
(a) introduce a parser bug (e.g. make the fence regex require a space) →
round-trip tests fail; (b) break an href in faq.html → link checker
fails; revert both, verify green again.

## Done criteria

- [ ] `bash scripts/verify.sh` exits 0 with ALL CHECKS PASSED
- [ ] Both sabotage checks fail loudly, then pass after revert
- [ ] Browser still boots part1 and the console round-trip is identical
      (the Step 1 refactor changed nothing observable)
- [ ] No commits; plans/README.md updated

## STOP conditions

- Step 1's hoist is impossible without touching DOM-coupled logic — stop
  and report which dependency blocks it rather than restructuring the
  player.
- node < 18 (no node:test) — report; do not add npm dependencies to a
  deliberately dependency-free repo.

## Maintenance notes

- Every future player change: run verify.sh; every content regeneration
  (plan 012): tests' structure counts may legitimately change — update the
  snapshot numbers WITH the content change in the same review.
- When the team ships the feature, wire verify.sh into a GitHub Action
  (`on: push: paths: docs/**`) — five-line follow-up documented in
  DEVELOPMENT.md.
