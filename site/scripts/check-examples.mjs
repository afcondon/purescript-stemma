// Checks the bundled examples without a browser: each one must lay out (with
// the library's default measurement and with a stand-in for the browser's,
// since Node has no canvas), the page's picture of it must build, and
// Graphviz's drawing of it must be in public/. Exits non-zero on a failure.
//
// Run after `spago build -p stemma-site`, from anywhere:
//   node site/scripts/check-examples.mjs

import { existsSync, statSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const site = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const output = join(site, "..", "output");

const { examples, graphvizVersion } = await import(join(output, "Site.Corpus", "index.js"));
const { layout, layoutWith, defaults } = await import(join(output, "Stemma.Layout", "index.js"));
const { picture, Marked } = await import(join(output, "Site.Picture", "index.js"));
const Label = await import(join(output, "Site.Label", "index.js"));

// The page measures with a canvas; this stands in for it with the same
// line breaking and line height, and a width per character.
const standIn = {
  ...defaults,
  measure: (written) => {
    const lines = Label.lines(written);
    return {
      width: Math.max(0, ...lines.map((line) => 7.8 * line.length)),
      height: Label.lineHeight * Math.max(1, lines.length),
    };
  },
};

const isRight = (either) => either.constructor.name === "Right";
let failures = 0;

for (const example of examples) {
  const problems = [];
  const started = performance.now();
  const plain = layout(example.dot);
  const measured = layoutWith(standIn)(example.dot);
  const took = performance.now() - started;
  if (!isRight(plain)) problems.push("layout: " + plain.value0);
  if (!isRight(measured)) problems.push("layoutWith: " + measured.value0);
  if (isRight(measured)) {
    try {
      picture(Marked.value)(measured.value0);
    } catch (error) {
      problems.push("picture: " + error.message);
    }
  }
  const svg = join(site, "public", example.graphvizPicture);
  if (!existsSync(svg) || statSync(svg).size === 0) problems.push("missing " + example.graphvizPicture);

  const drawing = isRight(measured) ? measured.value0 : null;
  const summary = drawing
    ? `${drawing.nodes.length} nodes, ${drawing.edges.length} edges, ${drawing.clusters.length} clusters`
    : "";
  if (problems.length === 0) {
    console.log(`ok    ${example.source.padEnd(48)} ${summary} (${took.toFixed(0)} ms)`);
  } else {
    failures += 1;
    console.log(`FAIL  ${example.source}\n      ${problems.join("\n      ")}`);
  }
}

console.log(`\n${examples.length - failures} of ${examples.length} examples ok; Graphviz pictures by ${graphvizVersion}`);
process.exit(failures === 0 ? 0 : 1);
