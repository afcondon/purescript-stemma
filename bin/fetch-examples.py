#!/usr/bin/env python3
"""Bring in Graphviz's own example graphs, which this repository does not keep.

They ship with Graphviz, so they are copied from the Graphviz installed
here: the graphs/directed and graphs/undirected folders beside its `dot`.
corpus/graphviz/FILES names the ones wanted and gives the SHA-256 of each,
so a different version of an example is reported and not used silently.

Then Graphviz is asked for its answers to them, which bin/check.py judges
against (oracle/graphviz/, also not kept in the repository).

    bin/fetch-examples.py
"""
import hashlib
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TARGET = ROOT / "corpus" / "graphviz"


def main():
    dot = shutil.which("dot")
    if not dot:
        sys.exit("no `dot` on the PATH: install Graphviz first")
    graphs = Path(dot).resolve().parent.parent / "share" / "graphviz" / "graphs"
    if not graphs.is_dir():
        sys.exit(f"this Graphviz has no example graphs at {graphs}")
    wanted = [line.split() for line in (TARGET / "FILES").read_text().splitlines() if line.strip()]
    copied, missing, changed = [], [], []
    for digest, name in wanted:
        source = next((p for p in (graphs / "directed" / name, graphs / "undirected" / name) if p.exists()), None)
        if source is None:
            missing.append(name)
        elif hashlib.sha256(source.read_bytes()).hexdigest() != digest:
            changed.append(name)
        else:
            shutil.copyfile(source, TARGET / name)
            copied.append(name)
    print(f"{len(copied)} of {len(wanted)} examples copied from {graphs}")
    if missing:
        print(f"  not in this Graphviz: {', '.join(missing)}")
    if changed:
        print(f"  different in this Graphviz, and left out: {', '.join(changed)}")
    if copied:
        print("asking Graphviz for its answers to them")
        subprocess.run([sys.executable, str(ROOT / "bin" / "check.py"), "--record"] + copied, cwd=ROOT,
                       stdout=subprocess.DEVNULL)
    sys.exit(1 if missing or changed else 0)


if __name__ == "__main__":
    main()
