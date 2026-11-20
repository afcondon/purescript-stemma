#!/usr/bin/env python3
"""Write corpus/layout/: random graphs for the layout passes. Unlike
corpus/generated/, these are plain DOT with sensible attributes: what
varies is the shape of the graph (trees, layered graphs, random DAGs,
graphs with cycles, loops and parallel edges, several components) and the
edge attributes that steer the ranking (weight, minlen, constraint, label).

Seeded, so the same files come out every time. The files are committed.
"""
import random
from pathlib import Path

OUT = Path(__file__).resolve().parent.parent / "corpus" / "layout"


def edge_attrs(rng, labels):
    parts = []
    if rng.random() < 0.25:
        parts.append(f"weight={rng.choice([0, 1, 2, 3, 5, 10])}")
    if rng.random() < 0.2:
        parts.append(f"minlen={rng.choice([0, 1, 2, 3])}")
    if rng.random() < 0.05:
        parts.append("constraint=false")
    if labels and rng.random() < 0.3:
        parts.append(f"label=L{rng.randint(0, 9)}")
    return (" [" + ", ".join(parts) + "]") if parts else ""


def tree(rng, n):
    return [(rng.randrange(i), i) for i in range(1, n)]


def dag(rng, n):
    count = rng.randint(n - 1, max(n, int(n * rng.uniform(1.0, 2.5))))
    return [tuple(sorted(rng.sample(range(n), 2))) for _ in range(count)] if n > 1 else []


def layered(rng, n):
    layer = [rng.randrange(max(2, n // 4)) for _ in range(n)]
    edges = []
    for _ in range(int(n * rng.uniform(1.0, 2.0))):
        a, b = rng.sample(range(n), 2)
        if layer[a] != layer[b]:
            edges.append((a, b) if layer[a] < layer[b] else (b, a))
    return edges


def cyclic(rng, n):
    edges = [(rng.randrange(n), rng.randrange(n)) for _ in range(int(n * rng.uniform(1.0, 2.2)))]
    if rng.random() < 0.5 and edges:
        edges += rng.sample(edges, min(len(edges), 3))   # parallel edges
    return edges


def ring(rng, n):
    return [(i, (i + 1) % n) for i in range(n)] + [(rng.randrange(n), rng.randrange(n)) for _ in range(n // 3)]


SHAPES = [("tree", tree), ("dag", dag), ("layered", layered), ("cyclic", cyclic), ("ring", ring)]


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    for old in OUT.glob("*.gv"):
        old.unlink()
    rng = random.Random(20261121)
    for i in range(300):
        name, shape = SHAPES[i % len(SHAPES)]
        n = rng.choice([2, 3, 4, 5, 6, 8, 10, 12, 16, 20, 25, 30, 40]) if i < 280 else rng.randint(100, 200)
        labels = rng.random() < 0.15
        edges = shape(rng, n)
        if rng.random() < 0.3 and n >= 6:     # a second component
            edges = [(a, b) for a, b in edges if (a < n // 2) == (b < n // 2)]
        lines = [f"digraph {name}{i:03d} {{"]
        order = list(range(n))
        if rng.random() < 0.5:
            rng.shuffle(order)
            lines += [f"  n{k};" for k in order]
        rng.shuffle(edges)
        lines += [f"  n{a} -> n{b}{edge_attrs(rng, labels)};" for a, b in edges]
        lines.append("}")
        (OUT / f"{name}-{i:03d}.gv").write_text("\n".join(lines) + "\n")
    print(f"wrote 300 files to {OUT}")


if __name__ == "__main__":
    main()
