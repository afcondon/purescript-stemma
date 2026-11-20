#!/usr/bin/env python3
"""Print one corpus file, and what dot and the core each made of it."""
import json, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parent.parent
path = next(ROOT.joinpath("corpus").rglob(sys.argv[1]))
print(path.read_text())
for label, file in (("dot ", ROOT / "oracle" / path.relative_to(ROOT / "corpus").with_suffix(".json")),
                    ("core", ROOT / "found" / path.relative_to(ROOT / "corpus").parent / (path.name + ".json"))):
    g = json.loads(file.read_text())
    if "error" in g:
        print(label, "ERROR", g["error"]); continue
    for s in g["subgraphs"]:
        print(label, "sub", s["name"], s["attributes"], s["nodes"],
              [(g["edges"][i]["tail"], g["edges"][i]["head"]) if isinstance(i, int) else tuple(i) for i in s["edges"]])
    for e in g["edges"]:
        print(label, "edge", e["tail"], e["head"], e["attributes"])
