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
