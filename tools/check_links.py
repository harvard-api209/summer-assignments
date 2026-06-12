#!/usr/bin/env python3
"""Internal link/anchor checker for the docs/ site.

Checks every relative href in docs/*.html against the pages and ids that
actually exist. FAQ question anchors (#q-...) are JS-generated at runtime
and are allowlisted.

Usage: python3 tools/check_links.py [--strict]
  --strict: exit 1 on any bad link (for verify.sh / CI).
"""
import re
import sys
from pathlib import Path

DOCS = Path(__file__).resolve().parent.parent / "docs"
ALLOW_FRAGMENT = re.compile(r"^q-")  # JS-generated FAQ anchors


def main():
    ids, links = {}, []
    for f in sorted(DOCS.glob("*.html")):
        html = f.read_text()
        ids[f.name] = set(re.findall(r'id="([^"]+)"', html))
        for href in re.findall(r'href="([^"]+)"', html):
            if href.startswith(("http", "mailto:")):
                continue
            links.append((f.name, href))

    bad = []
    for src, href in links:
        page, _, frag = href.partition("#")
        page = (page or src).split("?")[0]
        if not page.endswith(".html"):
            continue
        if page not in ids:
            bad.append((src, href, "missing page"))
            continue
        if frag and frag not in ids[page] and not ALLOW_FRAGMENT.match(frag):
            bad.append((src, href, "missing anchor"))

    if bad:
        for src, href, why in bad:
            print(f"BAD LINK {src}: {href} ({why})")
        if "--strict" in sys.argv:
            return 1
    else:
        print(f"links OK ({len(links)} internal links checked)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
