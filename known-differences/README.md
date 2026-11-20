# Known differences from Graphviz

Each file here is a case where this library and Graphviz do not agree, kept
with the reason. They are not in `corpus/`, which holds only what the two
are required to agree on.

## `graphviz-reads-this-two-ways.gv`

Graphviz 14.1.2 does not read this file the same way every time. Run

    gvpr -f bin/read.gvpr known-differences/graphviz-reads-this-two-ways.gv | md5sum

a dozen times and two different answers come out (on 2026-11-20, seven
runs gave one and one gave the other).

The file is a `strict` graph that comes to hold two edges between the same
two nodes, which the rules for `key` allow (see `connect` in
`core/src/Graphviz/Graph.purs`). A later statement with no key then has to
join one of them, and which one Graphviz joins varies from run to run. The
likely reason is that an edge made with a key is identified inside Graphviz
by a memory address, so the order it searches in changes with where the
process happens to be loaded; that is a guess from its behaviour, not from
its source.

This library joins the newest of the candidates, every time. On the
smallest case of the kind, `corpus/semantics/strict-joins-the-newest-of-two.gv`,
Graphviz does the same and does so reliably, so that file is in the corpus.

## `dot-draws-a-same-rank-edge-backwards.gv`

An edge between two nodes on the same rank is meant to run left to right,
tail first, and on small graphs dot does that. On this one (five edges) it
does not: `n5 -> n4` has `minlen=0`, the two end up on one rank, and dot
puts `n4` first, `n6` between them, and `n5` third, so the edge is drawn
from right to left over the top of `n6`.

    dot -Gphase=2 -Tdot known-differences/dot-draws-a-same-rank-edge-backwards.gv

This library always puts the tail on the left, unless that cannot be had
(the edges go round in a circle, or ask for a node to stand between two
nodes of one cluster). On the corpus, dot's order goes against that rule
on a few dozen graphs; `bin/check.py` prints how many.
