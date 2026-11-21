# Graphviz in PureScript

Graphviz's DOT language and its `dot` layout, written in PureScript from the
published algorithms, so that a graph can be laid out wherever PureScript
runs (a browser included) with no C and no WASM.

The installed Graphviz is the judge. Each stage is compared with what
Graphviz itself does on a corpus of DOT files, and a stage is not claimed
until that comparison says so.

## Using it

```purescript
import Graphviz.Layout (layout)
import Graphviz.Svg (render)

svg :: String -> Either String String
svg dot = render <$> layout dot
```

`layout` takes DOT text and returns a `Drawing`: boxes for the nodes, paths
for the edges, boxes for the clusters, in points. `Graphviz.Svg.render` turns
that into plain SVG text; a caller with its own renderer uses the `Drawing`
itself. `layoutWith` takes the spacing and, more to the point, a function
that measures text, since how wide a label is depends on the font and on
what draws it.

The ranks, the order within each rank and the place of everything across
the page come from the real passes. Edges are smooth curves that end on the
node outlines, with room kept for their labels and for loops, and the
graph's `size` is reported as a scale for whoever draws. Each edge takes the
shortest way through the free room beside the nodes it passes, and edges
between the same two nodes are spread apart. **Cluster boxes are not kept
clear**, so an edge can cut across a cluster it is not in. None of the geometry
is judged against Graphviz yet; it has been checked by eye on a few graphs.

## Where it stands

| Stage | State | Held to |
|---|---|---|
| Reading DOT: lexer, parser, and what the file means | done | 871 of 871 corpus files read exactly as Graphviz 14.1.2 reads them (866 the same, 5 rejected by both) |
| Which subgraphs are clusters | done | the same as dot draws, on 865 of 865 files (1,939 subgraphs) |
| Ranking (which layer each node is on), with `rank=same` and its relatives and with clusters | done, bar the cases below | 864 of 864 rankings proved optimal at every level; dot's own ranking equals the optimum on 860 and is worse on 4 |
| Turning cycles round before ranking | done | the same edges as dot on 328 of 329 graphs with cycles |
| Ordering within ranks, with clusters kept together and same-rank edges left to right | done; slow on large graphs | of 884 graphs, the same number of crossings as dot on 709, fewer on 92, more on 83; 12,391 crossings in all against dot's 12,855 |
| Coordinates | by network simplex: nodes, bends, cluster sides, room for labels and loops | nothing yet |
| Edge routing | curves pulled tight between the nodes they pass, ending on the node outlines; cluster boxes not kept clear | nothing yet |

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
reason.

- `corpus/graphviz/`: the 60 example graphs that ship with Graphviz.
- `corpus/semantics/`: about 60 small files, one per rule.
- `corpus/generated/`: 400 random files that lean on the order-dependent
  parts of the language (`bin/generate.py`, seeded).
- `corpus/layout/`: 300 random graphs with plain attributes, for the layout
  passes (`bin/generate-layout.py`, seeded).
- `corpus/ranking/`: the cases where dot's ranking is not optimal.
- `corpus/ranksets/`, `corpus/clusters/`, `corpus/cycles/`,
  `corpus/ordering/`: small files, one per rule.

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

## Turning cycles round

A ranking needs every edge to point down, so in each cycle some edge has to
be turned round, and it is then drawn pointing up the page. Any choice gives
a valid ranking; the choice shows in the picture. This follows dot's:
`core/src/Graphviz/Layout/Acyclic.purs` walks the graph depth first as dot
does, and matching it took three details found by experiment: where the walk
starts, that a node's edges are taken in the order of the nodes they lead
to, and that when an edge is turned round the last edge in its node's list
moves into the gap. The one corpus graph still different is a cluster that
also sets `rank=same`.

## Ordering within ranks

Long edges are cut into chains with a bend on every rank they pass, and
then the nodes and bends of each rank are ordered to leave few crossings:
`core/src/Graphviz/Layout/Order.purs`, the median-and-swap search of the
1993 paper, run on dot's schedule.

Two things hold in every order: the nodes of a cluster that share a rank
stand side by side (a rank is treated as a tree, and whole clusters are
moved and swapped as single things), and an edge between two nodes on one
rank has its tail on the left. The second cannot always be had: such edges
can go round in a circle, or ask for a node to stand between two nodes of
one cluster. Then one request is given up, and what remains is consistent
by construction. The judge checks both on every graph: 884 of 884.

An edge marked `constraint=false` has no say in the order and its crossings
are not counted, which is dot's rule too.

Nothing is optimal here, for dot or for this, so the two are compared by
result. Each orders the same ranking (dot's, from `dot -Gphase=2`), and the
crossings are counted. dot says how many it left (`dot -v`); to be sure the
two counts are of the same thing, the core's counter is run on dot's own
order wherever that order is fully known (no long edges), and must give
dot's number. It does on 277 of 277.

dot's own order, judged by the same two rules, never splits a cluster. It
does put the head of a same-rank edge on the left on some graphs
(`known-differences/` has a five-edge example).

It is slow on the largest corpus graphs (200 nodes): the crossing count and
the swaps are written the plain way and are quadratic.

Not done: an edge that passes a cluster on one side on one rank and the
other side on the next is not discouraged beyond its ordinary crossings, so
it can end up drawn across the cluster's box. dot discourages it heavily.

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
