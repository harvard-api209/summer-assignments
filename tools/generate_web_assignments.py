#!/usr/bin/env python3
"""Generate the browser-adapted assignments in docs/web-assignments/.

Source of truth: assignments/R Summer Assignment N.Rmd (the Posit Cloud
versions). NEVER hand-edit docs/web-assignments/*.Rmd — edit the canonical
assignment, then run this script.

Transformations (browser context: no Posit Cloud, no knitting):
  1. Remove "## Before you start*" and "## Why knit?" sections.
  2. Remove "### Knit checkpoint" sections.
  3. Rewrite ../data/ paths to data/ (the player stages CSVs at data/).
  4. Replace "## Before you submit" with the web checklist (per part).
  5. Targeted prose substitutions for remaining knitting/Posit phrasing
     (asserted: drift in the canonical text fails loudly).

Usage:
  python3 tools/generate_web_assignments.py          # regenerate in place
  python3 tools/generate_web_assignments.py --check  # exit 1 if outputs differ
"""
import re
import shutil
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC_DIR = ROOT / "assignments"
OUT_DIR = ROOT / "docs" / "web-assignments"
DATA_SRC = ROOT / "data"

CHECK_CORE = """## Before you submit

- Type your name in the panel at the top of the page so it appears in the
  exported file and its filename.
- Run your code chunks from top to bottom and fix any error or warning
  before you download. The file you export contains your code exactly as
  written.
- {ai_line}
{extras}- Download your completed `.Rmd` with the **Download my .Rmd** button and
  submit it to Canvas.
"""

EXTRAS = {
    1: "",
    2: "- Make sure each filter matches the written question.\n"
       "- Check missing values before interpreting poverty or inequality.\n",
    3: "- Make sure your plots have meaningful labels.\n"
       "- Do not make causal claims from descriptive graphs.\n"
       "- Check that peer countries and filters are coherent.\n",
    4: "- Check what one row represents before and after each summary.\n"
       "- Include denominators when summarizing sparse indicators.\n",
    5: "- Check that your peer comparison is coherent.\n"
       "- Check missing values for your main indicator.\n"
       "- Make sure your graphs have meaningful labels and avoid causal claims.\n",
}

# Exact prose substitutions per part. `old` MUST exist in the source —
# an AssertionError here means the canonical text changed and this rule
# needs updating (that is the drift alarm working, not a bug).
PROSE_SUBS = {
    1: [
        ("- read and fix a simple error message; and\n"
         "- explain why knitting checks reproducibility.",
         "- read and fix a simple error message."),
        ("The chunk below has an intentional error and is set to `eval=FALSE`, so it will\n"
         "not break knitting. Run the line manually, read the error, then fix the spelling\n"
         "in the next chunk.",
         "The chunk below has an intentional error so you can practice reading an error\n"
         "message. Run it, read the error, then fix the spelling in the next chunk."),
        ("read the prompt, run a small piece of code, check the output, explain what\n"
         "happened, and knit the file.",
         "read the prompt, run a small piece of code, check the output, and explain what\n"
         "happened."),
        ("This first assignment is a bridge from the website warm-up into the real Posit\n"
         "Cloud work.",
         "This first assignment is a bridge from the website warm-up into real coding\n"
         "work."),
    ],
    2: [
        ("try the code, check the output, explain what it means, and knit before moving\n"
         "on.",
         "try the code, check the output, and explain what it means before moving on."),
        ("The teaching team uses\n"
         "a cleaned CSV snapshot so everyone works with the same data and your assignment\n"
         "can knit reliably.",
         "The teaching team uses\n"
         "a cleaned CSV snapshot so everyone works with the same data and the assignment\n"
         "runs reliably."),
    ],
    3: [
        ("data, plot it, check the rows and labels, explain the pattern, and knit before\n"
         "moving on.",
         "data, plot it, check the rows and labels, and explain the pattern before\n"
         "moving on."),
    ],
    4: [
        ("summarize it, check the denominator and unit of analysis, explain the result,\n"
         "and knit before moving on.",
         "summarize it, check the denominator and unit of analysis, and explain the\n"
         "result before moving on."),
    ],
    5: [
        ("the data, produce output, explain what it means, and knit regularly. The",
         "the data, produce output, and explain what it means as you go. The"),
    ],
}


def remove_section(text, heading_pattern, level=2):
    """Remove a section: its heading line through the line before the next
    heading of depth <= level."""
    stop = r"^#{2,%d} " % level
    lines = text.split("\n")
    out, i = [], 0
    while i < len(lines):
        if re.match(heading_pattern, lines[i]):
            i += 1
            while i < len(lines) and not re.match(stop, lines[i]):
                i += 1
        else:
            out.append(lines[i])
            i += 1
    return "\n".join(out)


def generate_part(n: int) -> str:
    src = (SRC_DIR / f"R Summer Assignment {n}.Rmd").read_text()
    t = src
    t = remove_section(t, r"^## Before you start")
    t = remove_section(t, r"^## Why knit\?")
    t = remove_section(t, r"^### Knit checkpoint", level=3)
    t = t.replace("../data/", "data/")

    idx = t.index("## Before you submit")
    ai_line = ("Complete the memo and the AI-use reflection."
               if n == 5 else "Complete the AI-use note.")
    t = t[:idx] + CHECK_CORE.format(ai_line=ai_line, extras=EXTRAS[n])

    for old, new in PROSE_SUBS[n]:
        assert old in t, (
            f"part {n}: expected text not found (canonical assignment "
            f"changed?):\n---\n{old[:120]}\n---"
        )
        t = t.replace(old, new)

    leftovers = [
        line for line in t.split("\n")
        if re.search(r"Posit|[Kk]nit", line) and "knitr::opts_chunk" not in line
    ]
    assert not leftovers, f"part {n}: knit/Posit prose leaked: {leftovers[:3]}"
    return t


def generate_all(out_dir: Path) -> dict:
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "data").mkdir(exist_ok=True)
    counts = {}
    for n in range(1, 6):
        text = generate_part(n)
        (out_dir / f"part-{n}.Rmd").write_text(text)
        counts[n] = len(re.findall(r"^\[Write your", text, re.M))
    for csv in sorted(DATA_SRC.glob("*.csv")):
        shutil.copy2(csv, out_dir / "data" / csv.name)
    return counts


def main():
    check = "--check" in sys.argv
    if not check:
        counts = generate_all(OUT_DIR)
        print("Wrote part-1..5.Rmd + data/ to", OUT_DIR)
        print("Answer-box counts (keep docs/site.js PART_ANSWER_TOTALS in sync):",
              counts)
        return 0

    with tempfile.TemporaryDirectory() as tmp:
        tmp_dir = Path(tmp) / "web-assignments"
        generate_all(tmp_dir)
        diffs = []
        for n in range(1, 6):
            a = (tmp_dir / f"part-{n}.Rmd").read_text()
            b_path = OUT_DIR / f"part-{n}.Rmd"
            if not b_path.exists() or b_path.read_text() != a:
                diffs.append(f"part-{n}.Rmd")
        if diffs:
            print("DRIFT: regenerating would change:", ", ".join(diffs))
            print("Run: python3 tools/generate_web_assignments.py")
            return 1
        print("web-assignments are in sync with assignments/")
        return 0


if __name__ == "__main__":
    sys.exit(main())
