# Plan 002: Decouple the site from Posit Cloud — local-work path + single-source platform links

> **Executor instructions**: Follow this plan step by step. Run every
> verification command and confirm the expected result before moving to the
> next step. If anything in the "STOP conditions" section occurs, stop and
> report — do not improvise. When done, update the status row for this plan
> in `plans/README.md`.
>
> **Drift check (run first)**: `git diff --stat e2dfc1c..HEAD -- docs/`
> If any in-scope file changed since this plan was written, compare the
> "Current state" excerpts against the live code before proceeding; on a
> mismatch, treat it as a STOP condition.

## Status

- **Priority**: P2
- **Effort**: M
- **Risk**: LOW
- **Depends on**: plans/001-unify-part-counting.md (touches the same files;
  land 001 first to avoid merge conflicts)
- **Category**: tech-debt / direction
- **Planned at**: commit `e2dfc1c`, 2026-06-11

## Why this matters

A faculty reviewer's reaction to the site: *"my only reaction is how
confident you are that you'll be using posit cloud."* The site currently
hard-codes the platform in two ways: (1) **links** — `https://posit.cloud`
and the share link `https://posit.cloud/content/8155534` appear in raw
`href`s across 7 HTML files, so a platform change (different share link,
different service, or a move to local RStudio) means hand-editing every
page; (2) **no alternative path** — the FAQ answers "Do I need to install
R or RStudio?" with a flat "No", leaving students who prefer or need a
local setup (flaky internet, Posit Cloud outage, privacy constraints)
without guidance. After this plan, both platform URLs live in exactly one
place (`docs/site.js`), and the site offers a short, honest "prefer to
work locally?" path.

## Current state

Static site in `docs/`, served by GitHub Pages from branch `2026-refresh`
`/docs`. No build step. Conventions: 2-space-indent HTML, vanilla ES5-style
JS in `docs/site.js` (IIFE, `var`), cache busting via `?v=` query strings
on `site.css`/`site.js` (bump on change).

- `docs/site.js` top of file already defines one constant:

  ```js
  // site.js:11-15
  /* The share link creates a NEW temporary copy each visit, so it is only
     for the first visit. Returning students must reopen their saved copy
     from their own workspace, or they will work in (and lose) a copy. */
  var WORKSPACE_URL = "https://posit.cloud";
  ```

  The share link is NOT yet a constant.

- Hard-coded `href="https://posit.cloud..."` locations (verify with the
  grep in Step 1; counts as of `e2dfc1c`):
  - `docs/getting-started.html:81` — the share link
    `https://posit.cloud/content/8155534` ("First steps" list).
  - `docs/index.html` — workspace link in the "First visit vs. returning"
    section (`entry-returning` card).
  - `docs/faq.html` — workspace link in the temporary-copy FAQ answer.
  - `docs/submission.html` — workspace links in "Download from Posit
    Cloud" step 1 and the final actions row.
  - `docs/interactive-hour.html` — workspace link in the finish panel.
  - `docs/data.html` — workspace link in "Use it in your work".

- `docs/faq.html`, "Do I need to install R or RStudio?" article:

  ```html
  <h3>Do I need to install R or RStudio?</h3>
  <p>
    No. You can complete the summer assignments in Posit Cloud, which
    runs RStudio in your browser. During Math Camp, the teaching team
    will help students move from Posit Cloud to a local R/RStudio
    setup.
  </p>
  ```

- The repo root contains the actual project content students need
  locally: `assignments/` (5 `.Rmd` files) and `data/` (2 CSVs). The
  GitHub repo is public: `https://github.com/harvard-api209/summer-assignments`.
  GitHub serves a zip of the branch at
  `https://github.com/harvard-api209/summer-assignments/archive/refs/heads/2026-refresh.zip`.

## Commands you will need

| Purpose | Command | Expected on success |
|---------|---------|---------------------|
| Find platform links | `grep -rn 'href="https://posit.cloud' docs/` | see steps |
| Serve locally | `python3 -m http.server 8210 --directory docs` | site at :8210 |
| JS syntax check | `node --check docs/site.js` | exit 0 |

## Scope

**In scope**:
- `docs/site.js`
- `docs/index.html`, `docs/getting-started.html`, `docs/faq.html`,
  `docs/submission.html`, `docs/interactive-hour.html`, `docs/data.html`
- `?v=` bumps on all `docs/*.html`

**Out of scope** (do NOT touch):
- Renaming "Posit Cloud" in prose. The ~50 prose mentions are correct
  while the course uses Posit Cloud; this plan makes a future swap
  *mechanical*, it does not pretend the swap already happened.
- `assignments/*.Rmd` (plan 004 owns those).
- Building a full "local setup tutorial" (installing R, RStudio,
  tidyverse step-by-step). That is teaching-team content; this plan adds
  a pointer-level path only.

## Git workflow

- Branch off `2026-refresh`: `advisor/002-decouple-posit-platform`
- Short imperative commit subjects. Do NOT push or merge.

## Steps

### Step 1: Centralize both platform URLs in `site.js`

Next to `WORKSPACE_URL`, add:

```js
var PROJECT_SHARE_URL = "https://posit.cloud/content/8155534";
var REPO_ZIP_URL =
  "https://github.com/harvard-api209/summer-assignments/archive/refs/heads/2026-refresh.zip";
```

Add an init function (call it from the init block at the bottom of the
IIFE, alongside `markCurrentNavLink()` etc.):

```js
/* Single source of truth for platform links. Static hrefs in the HTML
   are the no-JS fallback and MUST stay correct; this rewrite exists so a
   future platform change is: update these constants, then sweep the
   static hrefs at leisure. */
function applyPlatformLinks() {
  var urls = {
    workspace: WORKSPACE_URL,
    project: PROJECT_SHARE_URL,
    zip: REPO_ZIP_URL
  };
  document.querySelectorAll("[data-platform-link]").forEach(function (a) {
    var url = urls[a.dataset.platformLink];
    if (url) { a.href = url; }
  });
}
```

**Verify**: `node --check docs/site.js` → exit 0.

### Step 2: Tag every platform link in the HTML

For each `href="https://posit.cloud..."` found by
`grep -rn 'href="https://posit.cloud' docs/`, add the matching attribute,
keeping the existing href as the no-JS fallback:

- Share link (`/content/8155534`) → `data-platform-link="project"`
- Workspace links (`https://posit.cloud`) → `data-platform-link="workspace"`

Example (`docs/getting-started.html:81`):

```html
<a href="https://posit.cloud/content/8155534" data-platform-link="project"
   target="_blank" rel="noopener">API-209 Posit Cloud project</a>
```

**Verify**:
`grep -rn 'href="https://posit.cloud' docs/ | grep -v data-platform-link`
→ no matches.

### Step 3: Add the "prefer to work locally?" path

1. In `docs/faq.html`, replace the install-question answer's first word
   and add the alternative (keep the article's `<h3>` unchanged so the
   JS-generated anchor id stays stable):

   ```html
   <h3>Do I need to install R or RStudio?</h3>
   <p>
     No — Posit Cloud runs RStudio in your browser and is the supported
     default. During Math Camp, the teaching team will help everyone move
     to a local R/RStudio setup.
   </p>
   <p>
     If you prefer to work locally now (or your internet connection makes
     a cloud editor painful), you can: install R and RStudio Desktop from
     <a href="https://posit.co/download/rstudio-desktop/" target="_blank"
        rel="noopener">posit.co/download</a>, download the course
     materials as a
     <a href="https://github.com/harvard-api209/summer-assignments/archive/refs/heads/2026-refresh.zip"
        data-platform-link="zip">zip file</a>, unzip it, and open the
     <code>.Rmd</code> files in the <code>assignments</code> folder. The
     folder structure and file paths are identical to the Posit Cloud
     project. Submission is the same either way: upload the completed
     <code>.Rmd</code> files to Canvas.
   </p>
   ```

2. In `docs/getting-started.html`, after the "Use a computer for the
   work" callout, add one pointer-level callout:

   ```html
   <div class="callout">
     <strong>Prefer to work locally?</strong> Posit Cloud is the supported
     default, but the assignments also run in RStudio Desktop. See the
     <a href="faq.html#q-do-i-need-to-install-r-or-rstudio">FAQ entry</a>
     for the short version.
   </div>
   ```

**Verify**: serve locally; the FAQ answer renders both paragraphs; the
getting-started callout links to the FAQ and the anchor resolves (the FAQ
page scrolls to the question).

### Step 4: Bump cache versions

`site.js?v=N` → `N+1` in all `docs/*.html` (check the current value first
— plan 001 may have already bumped it).

**Verify**: `grep -rho 'site\.js?v=[0-9]*' docs/*.html | sort -u` → exactly
one value.

## Test plan

Manual, on the served site:

1. With JS enabled: click "Open Your Workspace" (index), the FAQ zip link,
   and the getting-started project link — each resolves to the URL in the
   `site.js` constants (inspect `document.querySelector('[data-platform-link="project"]').href`).
2. With JS disabled: the same links still point at the correct (static)
   URLs.
3. Swap drill (then revert): change `WORKSPACE_URL` to
   `https://example.com`, reload, confirm every workspace link now points
   at example.com. Revert before committing.

## Done criteria

- [ ] `grep -rn 'href="https://posit.cloud' docs/ | grep -v data-platform-link` → no matches
- [ ] `grep -c 'data-platform-link' docs/site.js` → ≥ 1 (the rewrite function)
- [ ] FAQ contains the local-work paragraph; getting-started contains the callout
- [ ] Swap drill passes and was reverted
- [ ] `git status` shows only in-scope files modified
- [ ] `plans/README.md` status row updated

## STOP conditions

- Drift: the grep in Step 2 finds posit.cloud links in files not listed in
  "Current state" — map them first, then proceed only if they fit the
  workspace/project split.
- The teaching team has NOT confirmed the GitHub repo will remain public.
  If `curl -sI https://github.com/harvard-api209/summer-assignments | head -1`
  is not `HTTP/2 200`, the zip-download path is wrong — stop and report.
- You are tempted to rewrite prose mentions of "Posit Cloud" — out of
  scope; report the impulse instead.

## Maintenance notes

- **The platform-swap runbook this plan buys**: change the constants in
  `site.js`, test, then sweep static `href`s (`grep -rn 'posit.cloud' docs/`)
  and prose at leisure. Document the swap in the commit message.
- The FAQ local-work copy is advisor-drafted — the teaching team should
  review the wording before the site is announced to students (it sets
  support expectations).
- If plan 003 (webR) ships, revisit the FAQ answer: in-browser execution
  may become a third path for the warm-up specifically.
