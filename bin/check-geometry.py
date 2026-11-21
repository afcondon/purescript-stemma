#!/usr/bin/env python3
"""Judge the geometry: where the core puts nodes and how it draws edges.

A drawing has no single right answer, so this does not ask for dot's
coordinates. It checks rules any drawing should keep, and holds the core's
drawing beside dot's on measures where smaller is better.

Rules, checked on the core's drawing:
  - no two nodes overlap;
  - every node and cluster is inside the drawing;
  - every edge starts at its tail and ends at its head;
  - no edge label lies on a node.

Measures, for the core and for dot alike:
  - edges that pass through a node that is not one of their ends;
  - the area of the drawing;
  - the total length of the edges.

The two are not on equal terms for size: the core guesses the width of
text and dot measures it in Times, so node sizes differ.

    bin/check-geometry.py              every corpus file
    bin/check-geometry.py graphviz     one corpus folder
    bin/check-geometry.py --record     ask dot again and keep its answers

dot's answers are kept in oracle/geometry/, one file per corpus folder.
"""
import json
import math
import statistics
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CORPUS = ROOT / "corpus"
ORACLE = ROOT / "oracle" / "geometry"
FOUND = ROOT / "found" / "geometry"


def ask_dot(path):
    done = subprocess.run(["dot", "-Tjson", str(path)], capture_output=True)
    if done.returncode != 0:
        return {"error": done.stderr.decode(errors="replace").strip()[:200]}
    try:
        graph = json.loads(done.stdout.decode(errors="replace"))
    except ValueError:
        return {"error": "dot's JSON could not be read"}
    left, bottom, right, top = (float(v) for v in graph.get("bb", "0,0,0,0").split(","))
    nodes = {}
    for thing in graph.get("objects", []):
        if "pos" in thing and "width" in thing:
            x, y = (float(v) for v in thing["pos"].split(","))
            nodes[thing["_gvid"]] = [x, y, float(thing["width"]) * 72, float(thing["height"]) * 72]
    edges = []
    for edge in graph.get("edges", []):
        if "pos" not in edge:
            continue
        # The first piece of a multi-part edge; "s," and "e," mark arrow tips.
        points = [[float(v) for v in pair.split(",")] for pair in edge["pos"].split(";")[0].split()
                  if not pair.startswith(("s,", "e,"))]
        edges.append({"tail": edge["tail"], "head": edge["head"], "path": points})
    order = sorted(nodes)
    place = {gvid: index for index, gvid in enumerate(order)}
    return {"width": right - left, "height": top - bottom,
            "nodes": [nodes[gvid] for gvid in order],
            "edges": [{"tail": place[e["tail"]], "head": place[e["head"]], "path": e["path"]}
                      for e in edges if e["tail"] in place and e["head"] in place]}


def from_core(found):
    names = {json.dumps(node["name"], sort_keys=True): index for index, node in enumerate(found["nodes"])}
    return {"width": found["width"], "height": found["height"],
            "nodes": [node["box"] for node in found["nodes"]],
            "clusters": found["clusters"],
            "edges": [{"tail": names[json.dumps(e["tail"], sort_keys=True)],
                       "head": names[json.dumps(e["head"], sort_keys=True)],
                       "path": e["path"], "labelAt": e["labelAt"]} for e in found["edges"]]}


def along(path, steps=12):
    """Points along a run of cubic Bezier pieces."""
    points = []
    for start in range(0, len(path) - 3, 3):
        a, b, c, d = path[start:start + 4]
        for step in range(steps + 1):
            t = step / steps
            u = 1 - t
            points.append([u * u * u * a[i] + 3 * u * u * t * b[i] + 3 * u * t * t * c[i] + t * t * t * d[i]
                           for i in (0, 1)])
    return points or path


def inside(point, box, grow=1.0):
    """Inside the ellipse that fits the box, grown or shrunk."""
    x, y, width, height = box
    if width <= 0 or height <= 0:
        return False
    return ((point[0] - x) / (width / 2 * grow)) ** 2 + ((point[1] - y) / (height / 2 * grow)) ** 2 < 1


def through(drawing):
    """Edges that pass through a node that is not one of their ends."""
    count = 0
    for edge in drawing["edges"]:
        points = along(edge["path"])
        others = [box for index, box in enumerate(drawing["nodes"]) if index not in (edge["tail"], edge["head"])]
        if any(inside(point, box, 0.9) for box in others for point in points):
            count += 1
    return count


def length(drawing):
    total = 0.0
    for edge in drawing["edges"]:
        points = along(edge["path"])
        total += sum(math.dist(a, b) for a, b in zip(points, points[1:]))
    return total


def broken(drawing):
    """The rules the core's drawing breaks, as sentences."""
    found = []
    boxes = drawing["nodes"]
    overlapping = 0
    for i, a in enumerate(boxes):
        for b in boxes[i + 1:]:
            if abs(a[0] - b[0]) < (a[2] + b[2]) / 2 - 0.5 and abs(a[1] - b[1]) < (a[3] + b[3]) / 2 - 0.5:
                overlapping += 1
    if overlapping:
        found.append(f"{overlapping} pairs of nodes overlap")
    outside = sum(1 for x, y, w, h in boxes + drawing["clusters"]
                  if x - w / 2 < -0.5 or y - h / 2 < -0.5 or x + w / 2 > drawing["width"] + 0.5 or y + h / 2 > drawing["height"] + 0.5)
    if outside:
        found.append(f"{outside} nodes or clusters reach outside the drawing")
    adrift = 0
    for edge in drawing["edges"]:
        if len(edge["path"]) < 4 or (len(edge["path"]) - 1) % 3:
            adrift += 1
            continue
        for end, node in ((edge["path"][0], edge["tail"]), (edge["path"][-1], edge["head"])):
            x, y, w, h = boxes[node]
            if abs(end[0] - x) > w / 2 + 2 or abs(end[1] - y) > h / 2 + 2 or inside(end, boxes[node], 0.65):
                adrift += 1
    if adrift:
        found.append(f"{adrift} edge ends are not on their node's outline")
    covered = sum(1 for edge in drawing["edges"] if edge["labelAt"] and any(inside(edge["labelAt"], box) for box in boxes))
    if covered:
        found.append(f"{covered} edge labels lie on a node")
    return found


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    record = "--record" in sys.argv
    folders = sorted(p for p in CORPUS.iterdir() if p.is_dir() and (not args or p.name in args))
    rule_breakers, compared, not_run = [], [], 0
    for folder in folders:
        files = sorted(folder.glob("*.gv"))
        kept = ORACLE / (folder.name + ".json")
        if record or not kept.exists():
            ORACLE.mkdir(parents=True, exist_ok=True)
            kept.write_text(json.dumps({p.name: ask_dot(p) for p in files}, sort_keys=True) + "\n")
        theirs = json.loads(kept.read_text())
        out = FOUND / folder.name
        out.mkdir(parents=True, exist_ok=True)
        for old in out.glob("*.json"):
            old.unlink()
        # Four at a time: the large graphs take seconds each.
        batches = [files[start:start + 10] for start in range(0, len(files), 10)]
        with ThreadPoolExecutor(max_workers=4) as pool:
            list(pool.map(lambda batch: subprocess.run(["node", "run.mjs", "geometry", str(out)] + [str(p) for p in batch],
                                                       cwd=ROOT, check=True), batches))
        for path in files:
            found = json.loads((out / (path.name + ".json")).read_text())
            dot = theirs.get(path.name, {"error": "not recorded"})
            if "error" in found or "error" in dot:
                not_run += 1
                continue
            mine = from_core(found)
            rules = broken(mine)
            if rules:
                rule_breakers.append((f"{folder.name}/{path.name}", rules))
            compared.append({"name": f"{folder.name}/{path.name}",
                             "through": (through(mine), through(dot)),
                             "area": (mine["width"] * mine["height"], dot["width"] * dot["height"]),
                             "length": (length(mine), length(dot)),
                             "edges": len(mine["edges"])})

    total = len(compared)
    print("geometry:")
    print(f"  {total - len(rule_breakers)} of {total} drawings keep every rule")
    for name, rules in rule_breakers[:12]:
        print(f"    {name}: {'; '.join(rules)}")
    if len(rule_breakers) > 12:
        print(f"    ... and {len(rule_breakers) - 12} more")
    if total:
        edges = sum(c["edges"] for c in compared)
        print(f"  edges through a node that is not theirs, of {edges}: core {sum(c['through'][0] for c in compared)}, "
              f"dot {sum(c['through'][1] for c in compared)}")
        worst = sorted((c for c in compared if c["through"][0] > c["through"][1]), key=lambda c: c["through"][1] - c["through"][0])[:5]
        if worst:
            print("    most behind: " + ", ".join(f"{c['name']} ({c['through'][0]} against {c['through'][1]})" for c in worst))
        for key, what in (("area", "area of the drawing"), ("length", "total length of the edges")):
            ratios = [c[key][0] / c[key][1] for c in compared if c[key][1] > 0 and c[key][0] > 0]
            if ratios:
                quarters = statistics.quantiles(ratios, n=4)
                print(f"  {what}, core over dot, on {len(ratios)} drawings: median {statistics.median(ratios):.2f}, "
                      f"middle half {quarters[0]:.2f} to {quarters[2]:.2f}, largest {max(ratios):.2f}")
    if not_run:
        print(f"  NOT RUN  {not_run} graphs: Graphviz or the core rejects the file")
    sys.exit(1 if rule_breakers else 0)


if __name__ == "__main__":
    main()
