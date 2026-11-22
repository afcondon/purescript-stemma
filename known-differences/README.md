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
`core/src/Stemma/Graph.purs`). A later statement with no key then has to
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

## Differences in the drawing

These are not files but standing differences in what is drawn. None of them
changes which graph is read, how it is ranked or how its ranks are ordered.

**Shapes.** Graphviz has some sixty node shapes. Here an edge ends on one of
five outlines (ellipse, double ellipse, rectangle, diamond, or the bare
label), and every shape is taken as the nearest of them: a `house` or an
`octagon` is an ellipse, a `folder` is a rectangle. `corpus/graphviz/crazy.gv`
and `polypoly.gv` show it most. Circles are sized as circles and `point` as
a point.

**Text.** The layout does not know fonts; the caller says how big a piece
of text is. Graphviz measures in the font named, Times by default. So node
sizes differ a little everywhere, and with them the size of the drawing:
1.12 times dot's area at the median with the rough built-in guess.

**HTML and record labels.** A record's fields and a table's cells are
sized, so that the node is about the right size, but the layout places
nothing inside them: no ports on fields, no borders, no cell alignment.
The renderers show the text only.

**Ports.** `a:n -> b:sw` is read, and the ports are on the edge for a
caller to use, but every edge is drawn to the outline along the line it
arrives on, wherever the port says.

**Edges and clusters.** An edge may cut across the box of a cluster it is
not in. dot keeps most such edges out, though not all: in
`corpus/graphviz/clust5.gv` its `a -> z2` crosses a corner of the cluster
too.

**Edges through nodes.** Each edge takes the shortest way through the free
room beside the nodes it passes, where dot fits a curve inside a corridor
of boxes. On the corpus, 54 of 16,598 edges still pass through a node that
is not one of their ends; dot has 1.

**Same-rank edges between clusters.** Described under "Ordering within
ranks" in the main README: clusters keep one order across the drawing, and
an edge along a rank gives way to it. dot gives way on such edges too, on
more graphs than this library does (8 of the corpus against 2).

**Edge styles.** Curves only: no `splines=ortho`, `polyline` or `line`, no
`concentrate`, and arrowheads are the renderer's plain one whatever
`arrowhead` says.

**Graph attributes that scale or pad the page.** `size` is honoured, as a
scale reported to the renderer. `ratio`, `page`, `pad`, `margin`, `center`,
`rotate` and `landscape` are not.
