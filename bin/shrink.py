#!/usr/bin/env python3
"""Shrink a DOT file on which the core and Graphviz disagree to a small one
on which they still do, by deleting pieces of it (delta debugging).

    bin/shrink.py corpus/generated/random-087.gv
    bin/shrink.py corpus/generated/random-087.gv edge

Prints the shrunk file. Needs `gvpr` and a built harness.
"""
import json, subprocess, sys, tempfile
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import check

WORK = Path(tempfile.mkdtemp())
ONLY = sys.argv[2] if len(sys.argv) > 2 else None


def disagree(text):
    path = WORK / "t.gv"
    path.write_text(text)
    theirs = check.ask_graphviz(path)
    if "error" in theirs:
        return False
    subprocess.run(["node", "run.mjs", "read", str(WORK), str(path)], cwd=check.ROOT, check=True)
    mine = json.loads((WORK / "t.gv.json").read_text())
    if "error" in mine:
        return ONLY is None
    found = check.differences(check.normalise(mine), check.normalise(theirs))
    # With a second argument, every difference must be of that kind (its
    # line must start with it: "edge", "node", "subgraph"), so that the
    # shrinking cannot wander off to some other disagreement.
    return bool(found) and (ONLY is None or all(line.startswith(ONLY) for line in found))


def shrink(text):
    size = max(len(text) // 2, 1)
    while size >= 1:
        i, shrunk = 0, False
        while i < len(text):
            candidate = text[:i] + text[i + size:]
            if disagree(candidate):
                text, shrunk = candidate, True
            else:
                i += size
        if not shrunk or size == 1:
            if size == 1 and not shrunk:
                break
            size = max(size // 2, 1) if not shrunk else size
        if size > 1 and not shrunk:
            continue
        if size == 1 and shrunk:
            continue
    return text


text = Path(sys.argv[1]).read_text()
assert disagree(text), "they agree on this file"
print(shrink(text))
