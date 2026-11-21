#!/usr/bin/env python3
"""Hold the PureScript core to Graphviz itself.

For every DOT file in corpus/ this asks Graphviz what the file means and
asks the core the same, then compares: the graph's own attributes, every
node and its attributes, every edge and its attributes, and every subgraph
with its place in the nesting and its members.

Graphviz is asked through `gvpr` and bin/read.gvpr, which walk the graph as
its own library (cgraph) holds it with no layout run. `dot -Tdot_json` is
not used for this: it reports the graph after dot's layout has edited it
(clusters gain edges and lose nodes).

    bin/check.py              # the whole corpus
    bin/check.py unix.gv      # the named files
    bin/check.py --record     # rewrite oracle/ from the installed dot

The oracle's answers are kept in oracle/, with the Graphviz version that
gave them, so the comparison also runs where Graphviz is not installed.
Without --record nothing in oracle/ is written.

A file the comparison cannot be made for is NOT RUN, with the reason; it is
never counted as agreeing. The exit code is non-zero if any file differs.
"""
import json
import subprocess
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CORPUS = ROOT / "corpus"
ORACLE = ROOT / "oracle"
FOUND = ROOT / "found"

GVPR = ROOT / "bin" / "read.gvpr"


def unescape(field):
    """A field of bin/read.gvpr's output. Plain text comes back as a string,
    an HTML string as {"html": text}, which is how the core writes one and
    how oracle/ stores one."""
    if field.startswith("\\H"):
        return {"html": plain(field[2:])}
    return plain(field)


def from_core(value):
    """Text as read from JSON, made comparable: plain text stays a string,
    an HTML string becomes ("html", text), so the two are never equal."""
    if isinstance(value, dict) and set(value) == {"html"}:
        return ("html", value["html"])
    return value


def characters(value):
    return value[1] if isinstance(value, tuple) else value


def plain(field):
    out, i = [], 0
    while i < len(field):
        if field[i] == "\\" and i + 1 < len(field):
            out.append({"\\": "\\", "t": "\t", "n": "\n", "r": "\r"}.get(field[i + 1], field[i + 1]))
            i += 2
        else:
            out.append(field[i])
            i += 1
    return "".join(out)


def pairs(fields):
    return {plain(fields[i]): unescape(fields[i + 1]) for i in range(0, len(fields) - 1, 2)}


def ask_graphviz(path):
    """What cgraph makes of the file, through bin/read.gvpr."""
    done = subprocess.run(["gvpr", "-f", str(GVPR), str(path)], capture_output=True)
    try:
        text = done.stdout.decode("utf-8")
    except UnicodeDecodeError:
        text = done.stdout.decode("latin-1")
    errors = [l for l in done.stderr.decode("utf-8", "replace").splitlines() if l.startswith("Error")]
    if errors or not text:
        return {"error": errors[0] if errors else "gvpr printed nothing"}
    graph = {"nodes": [], "edges": [], "subgraphs": []}
    at = {}
    for line in text.split("\n"):
        if not line:
            continue
        fields = line.split("\t")
        kind = fields[0]
        if kind == "G":
            graph.update(name=unescape(fields[1]), directed=fields[2] == "1", strict=fields[3] == "1",
                         attributes=pairs(fields[4:]))
        elif kind == "S":
            at[fields[1]] = len(graph["subgraphs"])
            parts = fields[1].split("/")[1:]
            graph["subgraphs"].append({"name": unescape(parts[-1]), "path": [unescape(x) for x in parts],
                                       "attributes": pairs(fields[2:]), "nodes": [], "edges": []})
        elif kind == "M":
            graph["subgraphs"][at[fields[1]]]["nodes"].append(unescape(fields[2]))
        elif kind == "F":
            graph["subgraphs"][at[fields[1]]]["edges"].append([unescape(fields[2]), unescape(fields[3])])
        elif kind == "N":
            graph["nodes"].append({"name": unescape(fields[1]), "attributes": pairs(fields[2:])})
        elif kind == "E":
            graph["edges"].append({"tail": unescape(fields[1]), "head": unescape(fields[2]),
                                   "attributes": pairs(fields[3:])})
    return graph


def anonymous(name):
    # Graphviz names an unnamed graph or subgraph %1, %2, ...
    return name is None or (isinstance(name, str) and name.startswith("%"))


def nonempty(attrs):
    # Graphviz reports an attribute set to "" as absent.
    return {k: v for k, v in attrs.items() if characters(v) != ""}


def normalise(graph):
    """A graph as read from JSON, the core's or the oracle's, with its HTML
    strings made comparable."""
    def attrs(found):
        return {k: from_core(v) for k, v in found.items()}
    graph["name"] = from_core(graph["name"])
    graph["attributes"] = attrs(graph["attributes"])
    for node in graph["nodes"]:
        node["name"], node["attributes"] = from_core(node["name"]), attrs(node["attributes"])
    for edge in graph["edges"]:
        edge["tail"], edge["head"], edge["attributes"] = from_core(edge["tail"]), from_core(edge["head"]), attrs(edge["attributes"])
    for sub in graph["subgraphs"]:
        sub["name"], sub["attributes"] = from_core(sub["name"]), attrs(sub["attributes"])
        sub["nodes"] = [from_core(n) for n in sub["nodes"]]
        if "path" in sub:       # the oracle's shape
            sub["path"] = [from_core(x) for x in sub["path"]]
            sub["edges"] = [[from_core(t), from_core(h)] for t, h in sub["edges"]]
    return graph


def edge_ends(graph, tail, head):
    return (tail, head) if graph["directed"] else tuple(sorted((tail, head), key=repr))


def edge_key(graph, edge):
    return (edge_ends(graph, edge["tail"], edge["head"]), tuple(sorted(nonempty(edge["attributes"]).items(), key=repr)))


def core_path(graph, sub):
    out = []
    while sub is not None:
        out.append(sub["name"])
        sub = graph["subgraphs"][sub["parent"]] if sub["parent"] is not None else None
    return list(reversed(out))


def subgraph_key(path, graph, sub, member_ends):
    return (tuple("%" if anonymous(part) else part for part in path),
            tuple(sorted(nonempty(sub["attributes"]).items(), key=repr)),
            tuple(sorted(sub["nodes"], key=repr)),
            tuple(sorted(Counter(member_ends).items(), key=repr)))


def differences(mine, theirs):
    """What differs, as a list of sentences; empty if nothing does."""
    out = []
    for field in ("strict", "directed"):
        if mine[field] != theirs[field]:
            out.append(f"{field}: core {mine[field]}, dot {theirs[field]}")
    # Graphviz does not keep whether the graph's own name was an HTML string.
    if not anonymous(theirs["name"]) and characters(mine["name"]) != characters(theirs["name"]):
        out.append(f"graph name: core {mine['name']!r}, dot {theirs['name']!r}")
    if nonempty(mine["attributes"]) != nonempty(theirs["attributes"]):
        out.append(f"graph attributes: core {nonempty(mine['attributes'])}, dot {nonempty(theirs['attributes'])}")

    my_nodes = {n["name"]: nonempty(n["attributes"]) for n in mine["nodes"]}
    their_nodes = {n["name"]: nonempty(n["attributes"]) for n in theirs["nodes"]}
    for name in sorted(set(my_nodes) - set(their_nodes), key=repr):
        out.append(f"node {name!r}: only the core has it")
    for name in sorted(set(their_nodes) - set(my_nodes), key=repr):
        out.append(f"node {name!r}: only dot has it")
    for name in sorted(set(my_nodes) & set(their_nodes), key=repr):
        if my_nodes[name] != their_nodes[name]:
            out.append(f"node {name!r}: core {my_nodes[name]}, dot {their_nodes[name]}")
    if [n["name"] for n in mine["nodes"]] != [n["name"] for n in theirs["nodes"]] and not out:
        out.append("the nodes are the same but in a different order")

    my_edges = Counter(edge_key(mine, e) for e in mine["edges"])
    their_edges = Counter(edge_key(theirs, e) for e in theirs["edges"])
    for key in sorted(set(my_edges) | set(their_edges), key=repr):
        if my_edges[key] != their_edges[key]:
            ends, attrs = key
            out.append(f"edge {ends[0]!r} to {ends[1]!r} {dict(attrs)}: core has {my_edges[key]}, dot {their_edges[key]}")

    my_subs = Counter(
        subgraph_key(core_path(mine, s), mine, s,
                     [edge_ends(mine, mine["edges"][i]["tail"], mine["edges"][i]["head"]) for i in s["edges"]])
        for s in mine["subgraphs"])
    their_subs = Counter(
        subgraph_key(s["path"], theirs, s, [edge_ends(theirs, t, h) for t, h in s["edges"]])
        for s in theirs["subgraphs"])
    for key in sorted(set(my_subs) | set(their_subs), key=repr):
        if my_subs[key] != their_subs[key]:
            path, attrs, nodes, _ = key
            out.append(f"subgraph {'/'.join(map(str, path))} {dict(attrs)} of {len(nodes)} nodes: "
                       f"core has {my_subs[key]}, dot {their_subs[key]}")
    return out


def ask_dot_clusters(path):
    """Which subgraphs dot draws as clusters: those it gives a bounding
    box. Only subgraphs with nodes in them, since dot draws no box round
    nothing."""
    done = subprocess.run(["dot", "-Tjson0", str(path)], capture_output=True)
    if done.returncode != 0:
        return {"error": "dot failed"}
    try:
        raw = json.loads(done.stdout.decode("utf-8", "replace"))
    except json.JSONDecodeError:
        return {"error": "dot's output could not be read"}
    found = [[thing["name"], "bb" in thing] for thing in raw.get("objects", [])[:raw.get("_subgraph_cnt", 0)]
             if thing.get("nodes")]
    return {"subgraphs": found}


def check_clusters(files, record):
    """Clusters: does the core call the same subgraphs clusters as dot?

    dot is asked which subgraphs it drew a box round. It can only be asked
    after its layout, which first takes a node out of every cluster but the
    first it is in, and draws nothing for a cluster left empty. The core
    does not do that yet (it belongs with ranking clusters), so the check
    runs one way: every subgraph that still has nodes in dot's answer must
    be in the core's with the same verdict. A subgraph the core has and
    dot emptied is not counted against either.
    """
    agree, differ, subgraphs = 0, [], 0
    for path in files:
        target = ORACLE / path.relative_to(CORPUS).with_suffix(".clusters.json")
        if record:
            target.write_text(json.dumps(ask_dot_clusters(path), ensure_ascii=False) + "\n")
        found_file = FOUND / path.relative_to(CORPUS).parent / (path.name + ".json")
        if not target.exists() or not found_file.exists():
            continue
        theirs, mine = json.loads(target.read_text()), json.loads(found_file.read_text())
        if "error" in theirs or "error" in mine:
            continue

        def key(name, cluster):
            return ("%" if anonymous(name) else characters(from_core(name)), cluster)

        ours = Counter(key(sub["name"], sub["cluster"]) for sub in mine["subgraphs"] if sub["nodes"])
        dots = Counter(key(name, cluster) for name, cluster in theirs["subgraphs"])
        subgraphs += sum(dots.values())
        if not (dots - ours):
            agree += 1
        else:
            differ.append((path.name, sorted((dots - ours).items(), key=repr)))
    for name, only_dot in differ[:8]:
        print(f"DIFFERS  {name}: dot has, and the core does not: {only_dot}")
    print("clusters, against dot:")
    print(f"  {agree} of {agree + len(differ)} files: the same subgraphs are clusters ({subgraphs} subgraphs with nodes)"
          + (f"; {len(differ)} differ" if differ else ""))
    return not differ


def ask_dot_ranks(path):
    """The rank dot gives each node: `phase=1` stops dot after ranking and
    leaves the rank on each node."""
    done = subprocess.run(["dot", "-Gphase=1", "-Tdot_json", str(path)], capture_output=True)
    if done.returncode != 0:
        lines = done.stderr.decode("utf-8", "replace").strip().splitlines()
        return {"error": lines[0] if lines else "dot failed"}
    try:
        raw = json.loads(done.stdout.decode("utf-8", "replace"))
    except json.JSONDecodeError as problem:
        return {"error": f"dot's output could not be read: {problem}"}
    # In dot's order, which is the order the nodes were created in, and so
    # the core's order. Names would not do: `b` and `<b>` are two nodes.
    names, ranks = [], []
    for thing in raw.get("objects", [])[raw.get("_subgraph_cnt", 0):]:
        if "rank" not in thing:
            return {"error": f"dot gave node {thing['name']!r} no rank"}
        names.append(thing["name"])
        ranks.append(int(thing["rank"]))
    return {"names": names, "ranks": ranks}


def total(requirements, ranks):
    return sum(r["weight"] * (ranks[r["head"]] - ranks[r["tail"]]) for r in requirements)


def broken(requirements, ranks):
    return [r for r in requirements if ranks[r["head"]] - ranks[r["tail"]] < r["minimum"]]


def not_optimal(nodes, requirements, ranks, tree):
    """Why this ranking is not proved optimal by its tree; None if it is.

    The proof is the standard one for network simplex and uses nothing of
    the core but its answer. If every requirement holds, the tree's edges
    are tight, the tree spans every group of joined nodes, and no tree edge
    has a negative cut value, then no ranking has a smaller total.
    """
    for r in requirements:
        if ranks[r["head"]] - ranks[r["tail"]] < r["minimum"]:
            return f"requirement from node {r['tail']} to node {r['head']} at least {r['minimum']} is broken"
    group = {n: n for n in nodes}

    def find(n):
        while group[n] != n:
            group[n] = group[group[n]]
            n = group[n]
        return n

    for r in requirements:
        group[find(r["tail"])] = find(r["head"])
    groups = len({find(n) for n in nodes})
    if len(tree) != len(nodes) - groups:
        return f"the tree has {len(tree)} edges; a spanning one has {len(nodes) - groups}"
    joined = {n: [] for n in nodes}
    for i in tree:
        r = requirements[i]
        if ranks[r["head"]] - ranks[r["tail"]] != r["minimum"]:
            return f"tree edge from node {r['tail']} to node {r['head']} is not tight"
        joined[r["tail"]].append((i, r["head"]))
        joined[r["head"]].append((i, r["tail"]))
    for i in tree:
        r = requirements[i]
        side, pending = {r["tail"]}, [r["tail"]]
        while pending:
            n = pending.pop()
            for j, other in joined[n]:
                if j != i and other not in side:
                    side.add(other)
                    pending.append(other)
        if r["head"] in side:
            return "the tree has a cycle"
        cut = sum(q["weight"] * ((q["tail"] in side) - (q["head"] in side)) for q in requirements
                  if (q["tail"] in side) != (q["head"] in side))
        if cut < 0:
            return f"tree edge from node {r['tail']} to node {r['head']} has cut value {cut}"
    return None


def check_ranking(files, record):
    """Ranking: is the core's ranking right, and how does dot's compare?

    The ranking is the answer to an optimisation: the least possible sum,
    over the edges, of weight times length in ranks. Two things are checked.

    The core's ranking is proved optimal, graph by graph, from the spanning
    tree it returns with it (see not_optimal). That does not depend on dot.

    Then dot's ranking of the same graph is costed. To compare like with
    like the core is given the same cycles to work with: it turns round
    exactly the edges dot turned round (read off dot's ranks), and dot's
    ranks must satisfy every requirement the core read off the file, or
    the core has misread an attribute. dot's total can then equal the
    optimum or exceed it. It was expected always to equal it. It does not:
    on some graphs dot 14.1.2 stops at a worse ranking, and which graphs
    depends on the order the nodes are declared in.

    Separately, where a graph has cycles, the edges the core would turn
    round by itself are compared with the ones dot did.
    """
    usable = []
    for path in files:
        target = ORACLE / path.relative_to(CORPUS).with_suffix(".rank.json")
        if record:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(json.dumps(ask_dot_ranks(path), ensure_ascii=False, sort_keys=True) + "\n")
        if target.exists():
            usable.append((path, json.loads(target.read_text())))

    for folder in sorted({p.parent for p, _ in usable}):
        out = FOUND / folder.relative_to(CORPUS) / "rank"
        out.mkdir(parents=True, exist_ok=True)
        for old in out.glob("*.json"):
            old.unlink()
        mine = [(p, o) for p, o in usable if p.parent == folder]
        for path, oracle in mine:
            if "ranks" in oracle:
                (out / (path.name + ".oracle.json")).write_text(json.dumps(oracle["ranks"]))
        for start in range(0, len(mine), 200):
            subprocess.run(["node", "run.mjs", "rank", str(out)] + [str(p) for p, _ in mine[start:start + 200]],
                           cwd=ROOT, check=True)

    proved, wrong, not_run = 0, [], Counter()
    dot_equal, dot_worse = 0, []
    cyclic, same_turned, turned_differently = 0, 0, []
    for path, oracle in usable:
        mine = json.loads((FOUND / path.relative_to(CORPUS).parent / "rank" / (path.name + ".json")).read_text())
        if "error" in oracle or "error" in mine:
            not_run["Graphviz or the core rejects the file"] += 1
            continue
        if "unsupported" in mine:
            not_run[mine["unsupported"]] += 1
            continue
        own, theirs, names = mine["own"], oracle["ranks"], oracle["names"]
        if "given" not in mine:
            wrong.append((path.name, [f"dot ranks {len(theirs)} nodes and the core has a different number"]))
            continue
        given = mine["given"]
        problems = []
        for label, attempt in (("following dot", given), ("on its own", own)):
            for level in attempt["levels"]:
                why = not_optimal(list(range(level["variables"])), level["requirements"], level["values"], level["tree"])
                if why:
                    problems.append(f"the core's ranking, {label}, is not proved optimal: {why}")
        ours = dots = 0
        for level in given["levels"]:
            # dot's ranking, as numbers for this level's variables. Nodes
            # the core moves together must be together in dot's ranking.
            dots_values = [None] * level["variables"]
            for node, variable in enumerate(level["of"]):
                if variable is None:
                    continue
                here = theirs[node] - level["offset"][node]
                if dots_values[variable] is None:
                    dots_values[variable] = here
                elif dots_values[variable] != here and len(problems) < 4:
                    problems.append(f"the core moves {names[node]} together with other nodes; dot does not")
            # The slack of a crossing edge is not a node, so dot does not
            # report it. It is only ever pulled upwards, so its best place
            # is as low as its two requirements allow.
            for variable in range(level["variables"]):
                if level["slack"][variable]:
                    out = [r for r in level["requirements"] if r["tail"] == variable]
                    if any(r["head"] == variable for r in level["requirements"]) or not out \
                            or any(dots_values[r["head"]] is None for r in out):
                        problems.append("a slack variable is not of the shape expected")
                    else:
                        dots_values[variable] = min(dots_values[r["head"]] - r["minimum"] for r in out)
            if None in dots_values:
                problems.append("the core has a variable dot's ranking gives no number to")
                continue
            for r in broken(level["requirements"], dots_values)[:2]:
                problems.append(f"dot's ranks break a requirement the core read: variable {r['tail']} to {r['head']} "
                                f"at least {r['minimum']} (dot has {dots_values[r['head']] - dots_values[r['tail']]})")
            ours += total(level["requirements"], level["values"])
            dots += total(level["requirements"], dots_values)
        if problems:
            wrong.append((path.name, problems))
            continue
        proved += 1
        if dots == ours:
            dot_equal += 1
        else:
            dot_worse.append((path.name, ours, dots))
        if given["reversed"] or own["reversed"]:
            cyclic += 1
            if sorted(given["reversed"]) == sorted(own["reversed"]):
                same_turned += 1
            else:
                turned_differently.append(path.name)

    for name, problems in wrong:
        print(f"WRONG  {name}")
        for line in problems[:4]:
            print(f"    {line}")
    considered = proved + len(wrong)
    print("ranking:")
    print(f"  {proved} of {considered} graphs: the core's ranking is proved optimal"
          + (f"; {len(wrong)} are wrong" if wrong else ""))
    print(f"  {dot_equal} of {proved}: dot's ranking has the same total, so it is optimal too")
    if dot_worse:
        print(f"  {len(dot_worse)} of {proved}: dot's ranking is worse than the optimum: "
              + ", ".join(f"{name} ({dots} against {ours})" for name, ours, dots in dot_worse[:6])
              + (f" and {len(dot_worse) - 6} more" if len(dot_worse) > 6 else ""))
    print(f"  {same_turned} of {cyclic} graphs with cycles: the core turns round the same edges as dot")
    if turned_differently:
        print("    not the same on: " + ", ".join(turned_differently[:6])
              + (f" and {len(turned_differently) - 6} more" if len(turned_differently) > 6 else ""))
    for reason, count in not_run.most_common():
        print(f"  NOT RUN  {count} graphs: {reason}")
    return not wrong


def ask_dot_order(path):
    """Where dot puts each node (`phase=2` stops it after ordering and
    leaves `rank` and `order` on every node), and how many crossings it
    says it left (`-v` makes it say)."""
    done = subprocess.run(["dot", "-v", "-Gphase=2", "-Tdot_json", str(path)], capture_output=True)
    if done.returncode != 0:
        return {"error": "dot failed"}
    try:
        raw = json.loads(done.stdout.decode("utf-8", "replace"))
    except json.JSONDecodeError:
        return {"error": "dot's output could not be read"}
    said = [line for line in done.stderr.decode("utf-8", "replace").splitlines()
            if line.startswith("mincross ") and line.rstrip().endswith("secs.") and " crossings, " in line]
    if said:
        crossings = int(said[-1].rsplit(": ", 1)[1].split(" crossings")[0])
    elif not raw.get("edges"):
        crossings = 0       # dot says nothing about a graph with no edges
    else:
        return {"error": "dot did not say how many crossings it left"}
    ranks, orders = [], []
    for thing in raw.get("objects", [])[raw.get("_subgraph_cnt", 0):]:
        if "rank" not in thing or "order" not in thing:
            return {"error": "dot gave a node no rank or no order"}
        ranks.append(int(thing["rank"]))
        orders.append(int(thing["order"]))
    return {"ranks": ranks, "orders": orders, "crossings": crossings}


def check_order(files, record):
    """Ordering: how do the crossings the core leaves compare with dot's?

    There is no optimum to hold either to: both are searches that settle
    for a good order. So the comparison is of results. The core is given
    dot's ranking, orders it its own way, and its crossings are counted;
    dot says how many it left.

    The two counts have to be of the same thing. Where no edge spans more
    than one rank, dot's order of the nodes is the whole order, so the
    core's counter is run on it and must give the number dot said.
    """
    usable = []
    for path in files:
        target = ORACLE / path.relative_to(CORPUS).with_suffix(".order.json")
        if record:
            target.write_text(json.dumps(ask_dot_order(path)) + "\n")
        if target.exists():
            usable.append((path, json.loads(target.read_text())))
    for folder in sorted({p.parent for p, _ in usable}):
        out = FOUND / folder.relative_to(CORPUS) / "order"
        out.mkdir(parents=True, exist_ok=True)
        for old in out.glob("*.json"):
            old.unlink()
        mine = [(p, o) for p, o in usable if p.parent == folder]
        for path, oracle in mine:
            if "ranks" in oracle:
                (out / (path.name + ".oracle.json")).write_text(json.dumps(oracle))
        for start in range(0, len(mine), 200):
            subprocess.run(["node", "run.mjs", "order", str(out)] + [str(p) for p, _ in mine[start:start + 200]],
                           cwd=ROOT, check=True)

    fewer, same, more, not_run = [], 0, [], Counter()
    counted, miscounted = 0, []
    broken_ours, split_dots, backwards_dots = [], [], []
    between_ours, between_dots = [], []
    ours_total = dots_total = 0
    for path, oracle in usable:
        mine = json.loads((FOUND / path.relative_to(CORPUS).parent / "order" / (path.name + ".json")).read_text())
        if "error" in oracle or "error" in mine:
            not_run["Graphviz or the core rejects the file"] += 1
            continue
        if "unsupported" in mine:
            not_run[mine["unsupported"]] += 1
            continue
        if mine["dots"] is not None:
            counted += 1
            if mine["dots"] != oracle["crossings"]:
                miscounted.append((path.name, mine["dots"], oracle["crossings"]))
        # A same-rank edge between two clusters standing side by side is
        # counted apart: the clusters have one order for the whole drawing,
        # and such edges on two ranks can ask for both.
        within = mine["backwards"] - mine["backwardsBetweenClusters"]
        if mine["split"] or within:
            broken_ours.append((path.name, mine["split"], within))
        if mine["backwardsBetweenClusters"]:
            between_ours.append(path.name)
        if mine["dotsBackwardsBetweenClusters"]:
            between_dots.append(path.name)
        if mine["dotsSplit"]:
            split_dots.append(path.name)
        if mine["dotsBackwards"]:
            backwards_dots.append(path.name)
        ours, dots = mine["crossings"], oracle["crossings"]
        ours_total += ours
        dots_total += dots
        if ours < dots:
            fewer.append((path.name, ours, dots))
        elif ours == dots:
            same += 1
        else:
            more.append((path.name, ours, dots))
    compared = len(fewer) + same + len(more)
    print("ordering, against dot:")
    print(f"  {counted - len(miscounted)} of {counted} graphs with no long edges: the core counts dot's order as dot does")
    for name, here, said in miscounted[:5]:
        print(f"    MISCOUNT {name}: the core counts {here} crossings in dot's order; dot says {said}")
    print(f"  {compared - len(broken_ours)} of {compared} graphs: the core keeps every cluster together and every same-rank edge left to right, "
          "but for edges between two clusters side by side")
    for name, splits, wrong in broken_ours[:5]:
        print(f"    BROKEN {name}: {splits} clusters split, {wrong} same-rank edges the wrong way")
    print(f"  dot's own order, judged the same way: a cluster's nodes apart on {len(split_dots)} graphs"
          + (f" ({', '.join(split_dots[:4])})" if split_dots else "")
          + f"; a same-rank edge the other way on {len(backwards_dots)}"
          + (f" ({', '.join(backwards_dots[:4])})" if backwards_dots else ""))
    print(f"  a same-rank edge the other way between two clusters side by side: the core on {len(between_ours)} graphs"
          + (f" ({', '.join(between_ours[:4])})" if between_ours else "")
          + f", dot on {len(between_dots)}"
          + (f" ({', '.join(between_dots[:4])})" if between_dots else ""))
    print(f"  of {compared} graphs ordered: the core leaves the same number of crossings as dot on {same}, "
          f"fewer on {len(fewer)}, more on {len(more)}")
    print(f"  crossings in all: core {ours_total}, dot {dots_total}")
    worst = sorted(more, key=lambda x: x[2] - x[1])[:5]
    if worst:
        print("    most behind: " + ", ".join(f"{name} ({ours} against {dots})" for name, ours, dots in worst))
    for reason, count in not_run.most_common():
        print(f"  NOT RUN  {count} graphs: {reason}")
    return not miscounted and not broken_ours


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    record = "--record" in sys.argv
    files = sorted(p for p in CORPUS.rglob("*.gv") if not args or p.name in args)
    if not files:
        sys.exit("no corpus files match")

    if record:
        version = subprocess.run(["dot", "-V"], capture_output=True).stderr.decode().strip()
        ORACLE.mkdir(exist_ok=True)
        (ORACLE / "VERSION").write_text(version + "\n")
        for path in files:
            target = ORACLE / path.relative_to(CORPUS).with_suffix(".json")
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(json.dumps(ask_graphviz(path), indent=1, ensure_ascii=False, sort_keys=True) + "\n")
        print(f"recorded {len(files)} files from {version}")

    # One output directory per corpus directory: file names repeat across them.
    for folder in sorted({p.parent for p in files}):
        out = FOUND / folder.relative_to(CORPUS)
        out.mkdir(parents=True, exist_ok=True)
        for old in out.glob("*.json"):
            old.unlink()
        mine = [p for p in files if p.parent == folder]
        for start in range(0, len(mine), 200):
            subprocess.run(["node", "run.mjs", "read", str(out)] + [str(p) for p in mine[start:start + 200]],
                           cwd=ROOT, check=True)

    version = (ORACLE / "VERSION").read_text().strip() if (ORACLE / "VERSION").exists() else "no oracle recorded"
    agree, differ, both_reject, not_run = 0, [], 0, []
    for path in files:
        oracle_file = ORACLE / path.relative_to(CORPUS).with_suffix(".json")
        if not oracle_file.exists():
            not_run.append((path.name, "no oracle answer recorded; run with --record"))
            continue
        theirs = json.loads(oracle_file.read_text())
        mine = json.loads((FOUND / path.relative_to(CORPUS).parent / (path.name + ".json")).read_text())
        if "error" in theirs and "error" in mine:
            both_reject += 1
        elif "error" in theirs:
            differ.append((path.name, [f"Graphviz rejects it ({theirs['error']}); the core accepts it"]))
        elif "error" in mine:
            differ.append((path.name, [f"the core rejects it ({mine['error']}); Graphviz accepts it"]))
        else:
            found = differences(normalise(mine), normalise(theirs))
            if found:
                differ.append((path.name, found))
            else:
                agree += 1

    for name, found in differ:
        print(f"DIFFERS  {name}")
        for line in found[:6]:
            print(f"    {line}")
        if len(found) > 6:
            print(f"    ... and {len(found) - 6} more")
    for name, why in not_run:
        print(f"NOT RUN  {name}: {why}")
    print(f"reading DOT, against {version}:")
    print(f"  {agree} of {len(files)} files read the same as Graphviz"
          + (f", {both_reject} rejected by both" if both_reject else "")
          + f", {len(differ)} differ, {len(not_run)} not run")
    clustered = check_clusters(files, record)
    ranked = check_ranking(files, record)
    ordered = check_order(files, record)
    sys.exit(1 if differ or not_run or not ranked or not clustered or not ordered else 0)


if __name__ == "__main__":
    main()
