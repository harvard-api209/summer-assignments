// Round-trip tests for the assignment player's pure core.
// The invariant under test is what guarantees that browser students submit
// the same document as Posit Cloud students: buildRmdFrom(parseRmd(src))
// with untouched state reproduces src, and with edits reproduces src with
// exactly those substitutions.
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);
const player = require(join(here, "..", "docs", "assignment-player.js"));
const { parseRmd, buildRmdFrom, validateParse, safeName, safeFilePart } = player;

const EMPTY = { chunks: {}, answers: {} };
const norm = (s) => s.replace(/\n+$/, "");

// Structure snapshot — regenerate with:
//   python3 tools/generate_web_assignments.py   (prints answer counts)
//   node -e '...parseRmd...'                    (see docs/DEVELOPMENT.md)
const STRUCTURE = {
  1: { chunks: 25, answers: 7 },
  2: { chunks: 26, answers: 5 },
  3: { chunks: 16, answers: 4 },
  4: { chunks: 13, answers: 4 },
  5: { chunks: 13, answers: 8 },
};

const source = (n) =>
  readFileSync(join(here, "..", "docs", "web-assignments", `part-${n}.Rmd`), "utf8");

for (const n of [1, 2, 3, 4, 5]) {
  test(`part-${n}: identity round-trip`, () => {
    const src = source(n);
    const out = buildRmdFrom(parseRmd(src), EMPTY, "");
    assert.equal(norm(out), norm(src));
  });

  test(`part-${n}: structure counts`, () => {
    const parsed = parseRmd(source(n));
    assert.equal(parsed.editableCount, STRUCTURE[n].chunks);
    assert.equal(parsed.answerCount, STRUCTURE[n].answers);
    assert.equal(validateParse(parsed, source(n)).length, 0);
  });
}

test("substitution: edited chunk and answer land exactly once, rest unchanged", () => {
  const src = source(1);
  const parsed = parseRmd(src);
  const state = {
    chunks: { 0: "99 * 99  # my edit" },
    answers: { 0: "My answer about what R does." },
  };
  const out = buildRmdFrom(parsed, state, "");
  assert.match(out, /99 \* 99 {2}# my edit/);
  assert.match(out, /My answer about what R does\./);

  const srcPlaceholders = (src.match(/^\[Write your answer here\.\]$/gm) || []).length;
  const outPlaceholders = (out.match(/^\[Write your answer here\.\]$/gm) || []).length;
  assert.equal(outPlaceholders, srcPlaceholders - 1);

  // every line outside the two substitutions is unchanged
  const srcLines = norm(src).split("\n");
  const outLines = norm(out).split("\n");
  const changed = outLines.filter((l) => !srcLines.includes(l));
  assert.deepEqual(changed.sort(), ["99 * 99  # my edit", "My answer about what R does."].sort());
});

test("name lands sanitized in the YAML author", () => {
  const out = buildRmdFrom(parseRmd(source(1)), EMPTY, 'Ana "Quotes"\nNewline');
  const author = out.split("\n").find((l) => l.startsWith("author:"));
  assert.equal(author, 'author: "Ana Quotes Newline"');
});

test("safeName / safeFilePart strip hostile characters", () => {
  assert.equal(safeName('a\r\nb"c\\d'), "a b c d");
  assert.equal(safeFilePart("First/Last:v?2"), "First-Last-v-2");
});

test("validateParse flags malformed input", () => {
  const broken = source(1).replace(/\n```\n/, "\n"); // drop one closing fence
  const problems = validateParse(parseRmd(broken), broken);
  assert.ok(problems.includes("unbalanced code fences"));

  const noYaml = "just text\n\n```{r}\n1+1\n```\n";
  assert.ok(validateParse(parseRmd(noYaml), noYaml).includes("missing YAML header"));
});

test("empty student answer exports the original placeholder", () => {
  const parsed = parseRmd(source(1));
  const out = buildRmdFrom(parsed, { chunks: {}, answers: { 0: "   " } }, "");
  assert.equal(norm(out), norm(source(1)));
});
