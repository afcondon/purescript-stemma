#!/usr/bin/env python3
"""Shrink a DOT file, a line at a time, while the core and dot still turn
round different edges to break its cycles. For files with one statement
per line, such as those in corpus/layout/.

    bin/shrink-cycles.py corpus/layout/ring-269.gv
"""
import json, subprocess, sys, tempfile
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import check

WORK = Path(tempfile.mkdtemp())


def disagree(lines):
    path = WORK / "t.gv"
    path.write_text("\n".join(lines) + "\n")
    oracle = check.ask_dot_ranks(path)
    if "ranks" not in oracle:
        return False
    (WORK / "t.gv.oracle.json").write_text(json.dumps(oracle["ranks"]))
    subprocess.run(["node", "run.mjs", "rank", str(WORK), str(path)], cwd=check.ROOT, check=True)
    mine = json.loads((WORK / "t.gv.json").read_text())
    return "given" in mine and sorted(mine["given"]["reversed"]) != sorted(mine["own"]["reversed"])


lines = Path(sys.argv[1]).read_text().splitlines()
assert disagree(lines), "they agree on this file"
head, body, tail = lines[:1], lines[1:-1], lines[-1:]
changed = True
while changed:
    changed = False
    for i in range(len(body) - 1, -1, -1):
        candidate = body[:i] + body[i + 1:]
        if disagree(head + candidate + tail):
            body, changed = candidate, True
print("\n".join(head + body + tail))
