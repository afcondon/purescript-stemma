# Graphviz in PureScript

Graphviz's DOT language and its `dot` layout, written in PureScript from the
published algorithms, so that a graph can be laid out wherever PureScript
runs (a browser included) with no C and no WASM.

The installed Graphviz is the judge. Each stage is compared with what
Graphviz itself does on a corpus of DOT files, and a stage is not claimed
until that comparison says so.

## Where it stands

| Stage | State | Held to |
|---|---|---|
| Reading DOT: lexer, parser, and what the file means | done | 871 of 871 corpus files read exactly as Graphviz 14.1.2 reads them (866 the same, 5 rejected by both) |
| Which subgraphs are clusters | done | the same as dot draws, on 865 of 865 files (1,939 subgraphs) |
| Ranking (which layer each node is on), with `rank=same` and its relatives and with clusters | done, bar the cases below | 864 of 864 rankings proved optimal at every level; dot's own ranking equals the optimum on 860 and is worse on 4 |
| Turning cycles round before ranking | first version | the same edges as dot on 229 of 326 graphs with cycles |
| Ordering within layers | not started | |
| Coordinates | not started | |
| Edge routing | not started | |

`bin/check.py` prints the current figure; this table is only as fresh as its
last edit.

## Reading DOT

"What the file means" is more than parsing. A node's attributes depend on
which defaults were in force where and when it was first mentioned, a named
subgraph mentioned twice is one subgraph, and in a `strict` graph whether a
second edge between two nodes is merged, ignored or kept depends on its key
and on which subgraph the statement is in. `core/src/Graphviz/Graph.purs`
states the rules; they were found by experiment against Graphviz, and some
of them are not what its documentation says.

The comparison covers the graph's attributes, every node and edge with its
attributes, and every subgraph with its nesting and its members.

Text is plain or an HTML string (`<...>`), and the two are kept apart as
Graphviz keeps them: `<b>` and `b` are different nodes, and an attribute
value remembers which it was. The comparison includes that.

## The graph a caller gets

`Graphviz.Graph` is what the layout passes work on, and what another
library would use:

- `Graph` is opaque and made only by `fromDot`, so every edge joins two of
  its nodes and every member of a subgraph is in the graph.
- Node names, edge numbers and subgraph numbers are their own types
  (`NodeName`, `EdgeId`, `SubgraphId`).
- Every edge end has a place for a port (`tailPort`, `headPort`): a name
  the node defines, a compass point, or both. Nothing uses them yet; they
  are there so that ordering and coordinates can.
- Subgraphs form a tree (`parent`), each says whether it is a cluster, and
  `clusters` and `enclosingCluster` give the clusters and their nesting.
- Attributes stay text, since a file may set anything, and
  `Graphviz.Attributes` is the one place that reads a value as a number, a
  boolean, a port or a rank kind.

Known differences from Graphviz are in `known-differences/`, each with its
reason. There is one so far: a file Graphviz itself reads two different
ways from one run to the next.

- `corpus/graphviz/`: the 60 example graphs that ship with Graphviz.
- `corpus/semantics/`: about 60 small files, one per rule.
- `corpus/generated/`: 400 random files that lean on the order-dependent
  parts of the language (`bin/generate.py`, seeded).
- `corpus/layout/`: 300 random graphs with plain attributes, for the layout
  passes (`bin/generate-layout.py`, seeded).
- `corpus/ranking/`: the cases where dot's ranking is not optimal.
- `corpus/ranksets/`, `corpus/clusters/`: small files, one per rule of
  rank sets and of clusters.

A further 9,000 random files from three other seeds were compared on
2026-11-20. They found the HTML-string rule and the file in
`known-differences/`, and otherwise agreed; they are not kept.

## Ranking

Ranking is an optimisation: put every edge's head at least `minlen` ranks
below its tail, at the least total of weight times length. The core solves
it with the network simplex of Gansner, Koutsofios, North and Vo (1993), in
`core/src/Graphviz/Layout/NetworkSimplex.purs`, and returns with each answer
the spanning tree that proves it optimal. `bin/check.py` checks that proof
itself, in a few lines that use nothing of the core but its answer.

dot's ranking is then costed against that optimum. To compare like with
like, the core is given the cycles as dot broke them. `dot -Gphase=1` stops
dot after ranking and reports each node's rank, so no ranks have to be
guessed from coordinates.

**dot is not always optimal.** This was not expected. On
`digraph { b -> c [weight=30]; a -> b; a -> c [minlen=3] }` dot 14.1.2 puts
`b` on rank 1, stretching the weight-30 edge over two ranks (total 64, where
35 is possible). Declare `a` before `b` and it finds the optimum. The cases
are in `corpus/ranking/`. Why dot stops short is not known here; its source
has not been read. So "the same as dot" and "as good as possible" are two
different targets for this pass, and at present the core hits the second.

The solver is the plain one: it recomputes every cut value from its
definition each round, which is easy to see is right and slow on a large
graph. The largest graphs in the corpus have 200 nodes.

### Rank sets and clusters

A subgraph with `rank=same` is ranked as one thing; `min`, `max`, `source`
and `sink` pin a set to the top or bottom. A cluster is ranked on its own
first and then moves as a rigid block, and an edge that crosses a cluster's
border is a preference and not a requirement: it may fail, at ten times its
weight per rank. So the ranking is a tree of problems, one per level (the
graph, each cluster, each cluster within a cluster), and each level's answer
carries its own proof. `core/src/Graphviz/Layout/Rank.purs` states the
rules; like the reading rules they were found by experiment against `dot`.

For each level the judge also checks that dot's ranking fits the model: the
nodes the core moves together are together in dot's ranking, and dot's
ranks satisfy every requirement the core derived. On the corpus they always
do, which is the evidence that the model is dot's.

Not yet handled, and reported as NOT RUN: a `rank=same` subgraph that
reaches into a cluster beside it (1 corpus file), `newrank`, and graphs
that limit the solver (`nslimit`). A graph that asks for one node to be on
both the top and the bottom rank makes dot itself fail.

## How the comparison is made

```bash
spago build
bin/check.py            # compare with the answers recorded in oracle/
bin/check.py --record   # re-ask the installed Graphviz first
```

Graphviz is asked through `gvpr` and `bin/read.gvpr`, which walk the graph
as Graphviz's own library holds it, with no layout run. (`dot -Tdot_json`
looks like the obvious oracle and is not one: it reports the graph after
dot's layout has added edges to clusters and removed nodes from them.) The
answers are kept in `oracle/` with the version that gave them, so the
comparison runs without Graphviz installed.

`bin/show.py FILE` prints one file with both readings. `bin/shrink.py FILE`
reduces a file the two disagree on to a small one they still disagree on.

## Layout

- `core/`: the library. Pure PureScript, no foreign code.
- `src/Main.purs`: a Node program that runs the core over files and writes
  what it found, for `bin/check.py`.

## Licence and provenance

MIT; see `LICENSE`. That covers everything here except the files in
`corpus/graphviz/`, which are Graphviz's (see below).

No code here is copied from Graphviz, and Graphviz's source was not read
while writing it. The algorithms come from the 1993 paper; the rules for
what a DOT file means and how dot ranks come from running the `dot` and
`gvpr` programs on small inputs and watching what they do, and each rule is
held to those programs by `bin/check.py`.

One qualification, so that this is not overstated: the code was written by
a language model (Claude), which has general knowledge of how dot works
from its training. Some of the guesses that the experiments then confirmed
(for instance, that an edge crossing a cluster's border is a soft
constraint) started from that knowledge and not from a blank sheet. So this
is an independent implementation checked against behaviour, and not a
clean-room one in the strict sense of the term.

The files in `corpus/graphviz/` are Graphviz's own example graphs and carry
its licence (EPL 1.0); see the notice there. They are test input, and could
equally be fetched from Graphviz when the tests run.
