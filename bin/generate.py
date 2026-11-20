#!/usr/bin/env python3
"""Write corpus/generated/: random DOT files that lean on the parts of the
language where meaning depends on order and nesting (defaults, reopened
subgraphs, strict, keys, subgraph endpoints, ports, odd spellings).

Seeded, so the same files come out every time. The files are committed;
this script is how they were made.
"""
import random
from pathlib import Path

OUT = Path(__file__).resolve().parent.parent / "corpus" / "generated"
NODES = ["a", "b", "c", "d", "e", "f", "g", "h", "n1", "n2", '"two words"', "-1", "3.5", "é", '"q\\"uote"', "_x"]
SUBGRAPHS = ["s", "t", "u", "cluster_a", "cluster_b"]
KEYS = ["color", "shape", "k", "weight", "label", "rank", "style"]
VALUES = ["red", "blue", "box", "1", "2", '"x y"', '""', "same", "<b>", '"line\\nbreak"']
PORTS = ["", "", "", ":n", ":p", ":p:sw"]


def attrs(rng, least=0):
    n = rng.randint(least, 3)
    if n == 0:
        return ""
    items = [f"{rng.choice(KEYS)}={rng.choice(VALUES)}" for _ in range(n)]
    return " [" + rng.choice([", ", "; ", " "]).join(items) + "]"


def endpoint(rng, depth, op, allow_port=True):
    if depth < 3 and rng.random() < 0.15:
        return subgraph(rng, depth + 1, op, brief=True)
    return rng.choice(NODES) + (rng.choice(PORTS) if allow_port else "")


def subgraph(rng, depth, op, brief=False):
    head = rng.choice(["", "", f"subgraph {rng.choice(SUBGRAPHS)} ", "subgraph "])
    body = statements(rng, depth, op, rng.randint(0, 2 if brief else 5))
    return head + "{ " + body + " }"


def statement(rng, depth, op):
    roll = rng.random()
    if roll < 0.30:
        ends = [endpoint(rng, depth, op) for _ in range(rng.randint(2, 4))]
        key = f" [key={rng.choice(['k1', 'k2'])}]" if rng.random() < 0.1 else ""
        return f" {op} ".join(ends) + attrs(rng) + key
    if roll < 0.50:
        return rng.choice(NODES) + attrs(rng)
    if roll < 0.68:
        return rng.choice(["node", "edge", "graph", "NODE", "Edge"]) + attrs(rng, least=1)
    if roll < 0.78:
        return f"{rng.choice(KEYS)}={rng.choice(VALUES)}"
    if depth < 3:
        return subgraph(rng, depth + 1, op)
    return rng.choice(NODES)


def statements(rng, depth, op, count):
    return rng.choice(["; ", " ", "\n", ";\n"]).join(statement(rng, depth, op) for _ in range(count))


def main():
    # With arguments (count, seed, directory name under corpus/) this writes
    # a throwaway set for a wider search; without, the committed one.
    import sys
    count, seed, out = 400, 20261120, OUT
    if len(sys.argv) == 4:
        count, seed, out = int(sys.argv[1]), int(sys.argv[2]), OUT.parent / sys.argv[3]
    out.mkdir(parents=True, exist_ok=True)
    for old in out.glob("*.gv"):
        old.unlink()
    rng = random.Random(seed)
    for i in range(count):
        directed = rng.random() < 0.7
        strict = rng.random() < 0.25
        op = "->" if directed else "--"
        text = (("strict " if strict else "") + ("digraph" if directed else "graph")
                + rng.choice([" G", "", ' "g 2"']) + " {\n"
                + statements(rng, 0, op, rng.randint(3, 14)) + "\n}\n")
        (out / f"random-{i:04d}.gv").write_text(text)
    print(f"wrote {count} files to {out}")


if __name__ == "__main__":
    main()
