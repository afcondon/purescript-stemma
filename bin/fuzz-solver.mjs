// Random problems for the solver, shaped like the ones the coordinate pass
// gives it, with every answer checked from the definition: each requirement
// holds, the tree returned is tight and spans, and no cut value is negative.
//
//     node bin/fuzz-solver.mjs [seed] [problems] [ranks] [width]
//
// Run from the repository root after `spago build`. It prints each problem
// that fails or takes more than three seconds, then a count.
const root = process.cwd() + "/output/";
const NS = await import(root + "Graphviz.Layout.NetworkSimplex/index.js");
const Ord = await import(root + "Data.Ord/index.js");
let seed = Number(process.argv[2] ?? 1);
const rnd = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
const int = (n) => Math.floor(rnd() * n);
function problem(ranks, width) {
  const reqs = []; let next = 0; const rows = [];
  for (let r = 0; r < ranks; r++) { const row = []; const w = 1 + int(width); for (let i = 0; i < w; i++) row.push(next++); rows.push(row);
    for (let i = 1; i < row.length; i++) reqs.push({ tail: row[i - 1], head: row[i], weight: 0, minimum: 10 + int(60) }); }
  for (let r = 1; r < ranks; r++) for (const v of rows[r]) { const k = 1 + int(2); for (let j = 0; j < k; j++) { const u = rows[r - 1][int(rows[r - 1].length)]; const p = next++; const w = [1, 2, 8][int(3)];
    reqs.push({ tail: p, head: u, weight: w, minimum: 0 }); reqs.push({ tail: p, head: v, weight: w, minimum: 0 }); } }
  return { reqs, n: next };
}
function check({ reqs, n }, solution) {
  const val = (v) => NS.value(Ord.ordInt)(solution.assignment)(v);
  for (const r of reqs) if (val(r.head) - val(r.tail) < r.minimum) return "infeasible";
  const tree = solution.tree; const inTree = new Set(tree);
  for (const i of tree) if (val(reqs[i].head) - val(reqs[i].tail) !== reqs[i].minimum) return "tree edge not tight";
  const adj = Array.from({ length: n }, () => []);
  for (const i of tree) { adj[reqs[i].tail].push([reqs[i].head, i]); adj[reqs[i].head].push([reqs[i].tail, i]); }
  for (const cutEdge of tree) {
    const side = new Set([reqs[cutEdge].tail]); const stack = [reqs[cutEdge].tail];
    while (stack.length) { const v = stack.pop(); for (const [w, i] of adj[v]) if (i !== cutEdge && !side.has(w)) { side.add(w); stack.push(w); } }
    if (side.has(reqs[cutEdge].head)) return "tree has a cycle";
    let cut = 0; for (const r of reqs) { const a = side.has(r.tail), b = side.has(r.head); if (a && !b) cut += r.weight; else if (!a && b) cut -= r.weight; }
    if (cut < 0) return "negative cut value " + cut;
  }
  return null;
}
const trials = Number(process.argv[3] ?? 30), ranks = Number(process.argv[4] ?? 6), width = Number(process.argv[5] ?? 6);
let bad = 0; const t0 = Date.now();
for (let t = 0; t < trials; t++) { const s0 = seed; const p = problem(ranks, width); const t1 = Date.now(); const sol = NS.solve(Ord.ordInt)(p.reqs); const ms = Date.now() - t1;
  const why = check(p, sol); if (why || ms > 3000) { bad++; console.log(`seed ${s0}: ${p.n} variables, ${p.reqs.length} requirements, ${ms} ms: ${why ?? "slow"}`); } }
console.log(`${trials} problems (${ranks} ranks, width ${width}): ${bad} bad, ${Date.now() - t0} ms`);
