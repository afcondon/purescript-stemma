// The Fairport Convention family tree: facts from MusicBrainz, and a DOT
// file drawn from them.
//
// MusicBrainz's core data, which includes artists, their begin and end
// dates and the relationships between them, is CC0
// (https://musicbrainz.org/doc/About/Data_License and
// https://musicbrainz.org/doc/MusicBrainz_Database). This script reads only
// "member of band" relationships and the bands' own begin and end dates.
//
// It asks the web service for Fairport Convention, then for each of its
// members, then for each band in `bands` below, and keeps every member of
// those bands. It writes:
//
//   site/examples/fairport/facts.json   what was read, trimmed to the
//                                        memberships used, with the MBIDs
//   site/examples/fairport/fairport.gv  the family tree as DOT
//
// The web service allows about one request a second from one address, so
// requests are spaced 1.2 s apart and a 503 is retried after a pause.
// Answers are kept in a cache directory so that rerunning the script to
// change the drawing does not ask again.
//
//   node site/scripts/fairport.mjs [--cache <dir>] [--offline]
//
// `--offline` redraws the DOT from facts.json without the network.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

const site = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const destination = join(site, "examples", "fairport");

const argumentsGiven = process.argv.slice(2);
const option = (name) => {
  const at = argumentsGiven.indexOf(name);
  return at >= 0 ? argumentsGiven[at + 1] : undefined;
};
const offline = argumentsGiven.includes("--offline");
const cache = option("--cache") ?? join(tmpdir(), "purescript-graphviz-musicbrainz");

// A name for the program and nothing else: no person, address or URL.
const userAgent = "purescript-graphviz-demo/0.1";

const fairport = "cc60d937-e80b-4c66-8aa2-e89cb57c8b08";

// The bands drawn, by MBID, with a colour for each. Colour is the only
// thing that tells the bands apart, since the layout cannot put a band's
// line-ups in a cluster: a `rank=same` set that reaches into a cluster is
// not handled (README, "Rank sets and clusters").
//
// `from` is the first year drawn for a band that only enters the story
// later, so that a long-lived band does not bring in line-ups that have
// nothing to do with Fairport.
const bands = [
  { mbid: fairport, label: "Fairport Convention", colour: "#c0392b" },
  { mbid: "14a82daf-2b80-4317-9e84-99f92f0a96b4", label: "Eclection", colour: "#6d4c41" },
  { mbid: "68c6926c-03e0-4afc-9f54-2190b2c8d468", label: "Steeleye Span", colour: "#1f5fa8" },
  { mbid: "dcc682d6-f7cb-4863-8a34-aaea25d84e2f", label: "Matthews’ Southern Comfort", colour: "#b9770e" },
  { mbid: "bc383eff-8ddc-488a-8a92-2e638c708c7f", label: "Fotheringay", colour: "#2e7d32" },
  { mbid: "a63b0be7-151d-497c-b564-dd6a3d8c6dc0", label: "The Albion Band", colour: "#8e44ad" },
  { mbid: "07c174b0-bf87-4b94-a31d-ef9172300d31", label: "The Bunch", colour: "#00838f" },
  { mbid: "28043a28-73ac-4bf4-b051-79f28df9351e", label: "Plainsong", colour: "#ad1457", from: 1972 },
  { mbid: "ece57992-dc2e-4f67-a269-fa43626c1a3d", label: "Jethro Tull", colour: "#546e7a", from: 1979 },
];

// The years drawn. Fairport's later line-ups would double the tree.
const firstYear = 1966;
const lastYear = 1980;

// Some memberships in MusicBrainz have no dates, and a membership with no
// dates cannot be put on the page. Where the band itself lasted three years
// or less, an undated member is taken to have been in it throughout. The
// rest are filled in here, by hand, from the sources named, and marked in
// facts.json as not from MusicBrainz. Years are as the sources give them;
// "to" is the year the person left, as in MusicBrainz.
const supplement = [
  { band: "Steeleye Span", person: "Martin Carthy", begin: "1970", end: "1972", source: "https://en.wikipedia.org/wiki/Steeleye_Span" },
  { band: "Steeleye Span", person: "Peter Knight", begin: "1970", end: "1977", source: "https://en.wikipedia.org/wiki/Steeleye_Span" },
  { band: "Steeleye Span", person: "Rick Kemp", begin: "1972", end: null, source: "https://en.wikipedia.org/wiki/Steeleye_Span" },
  { band: "Steeleye Span", person: "Nigel Pegrum", begin: "1973", end: null, source: "https://en.wikipedia.org/wiki/Steeleye_Span" },
  { band: "Steeleye Span", person: "Martin Carthy", begin: "1977", end: "1978", source: "https://en.wikipedia.org/wiki/Steeleye_Span" },
  { band: "Steeleye Span", person: "John Kirkpatrick", begin: "1977", end: "1978", source: "https://en.wikipedia.org/wiki/Steeleye_Span" },
  { band: "Matthews’ Southern Comfort", person: "Iain Matthews", begin: "1970", end: "1971", source: "https://en.wikipedia.org/wiki/Matthews_Southern_Comfort" },
  { band: "Matthews’ Southern Comfort", person: "Mark Griffiths", begin: "1970", end: "1971", source: "https://en.wikipedia.org/wiki/Matthews_Southern_Comfort" },
  { band: "Matthews’ Southern Comfort", person: "Gordon Huntley", begin: "1970", end: "1971", source: "https://en.wikipedia.org/wiki/Matthews_Southern_Comfort" },
  { band: "Matthews’ Southern Comfort", person: "Andy Leigh", begin: "1970", end: "1971", source: "https://en.wikipedia.org/wiki/Matthews_Southern_Comfort" },
  { band: "Matthews’ Southern Comfort", person: "Ray Duffy", begin: "1970", end: "1971", source: "https://en.wikipedia.org/wiki/Matthews_Southern_Comfort" },
  { band: "The Albion Band", person: "Ashley Hutchings", begin: "1971", end: null, source: "https://en.wikipedia.org/wiki/The_Albion_Band" },
  // Assembled by Trevor Lucas to make one album, Rock On (1972).
  ...["Trevor Lucas", "Sandy Denny", "Gerry Conway", "Ashley Hutchings", "Dave Mattacks", "Richard Thompson"].map((person) => (
    { band: "The Bunch", person, begin: "1972", end: "1972", source: "https://en.wikipedia.org/wiki/The_Bunch" })),
];

// --- Reading MusicBrainz ---------------------------------------------------

let lastRequest = 0;
const pause = (milliseconds) => new Promise((done) => setTimeout(done, milliseconds));

const artist = async (mbid) => {
  mkdirSync(cache, { recursive: true });
  const kept = join(cache, `${mbid}.json`);
  if (existsSync(kept)) return JSON.parse(readFileSync(kept, "utf8"));
  for (let attempt = 0; attempt < 6; attempt++) {
    const wait = lastRequest + 1200 + attempt * 3000 - Date.now();
    if (wait > 0) await pause(wait);
    lastRequest = Date.now();
    const response = await fetch(`https://musicbrainz.org/ws/2/artist/${mbid}?inc=artist-rels&fmt=json`, {
      headers: { "User-Agent": userAgent, Accept: "application/json" },
    });
    if (response.status === 503) continue;
    if (!response.ok) throw new Error(`MusicBrainz answered ${response.status} for ${mbid}`);
    const body = await response.json();
    writeFileSync(kept, JSON.stringify(body));
    return body;
  }
  throw new Error(`MusicBrainz kept refusing ${mbid}`);
};

const year = (date) => (date ? Number(date.slice(0, 4)) : undefined);

// Every "member of band" relationship of a band, from the band's side.
// The same relationship is sometimes entered twice; one copy is kept.
const membershipsOf = (band) => {
  const seen = new Set();
  return band.relations
    .filter((relation) => relation.type === "member of band" && relation.direction === "backward")
    .map((relation) => ({
      band: band.name,
      person: relation.artist.name,
      personId: relation.artist.id,
      begin: relation.begin ?? null,
      end: relation.end ?? null,
    }))
    .filter((membership) => {
      const key = [membership.personId, membership.begin, membership.end].join();
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
};

const gather = async () => {
  // The members of Fairport are asked for too, so that the cache holds
  // what a wider choice of bands would need; only `bands` is drawn.
  const seed = await artist(fairport);
  for (const membership of membershipsOf(seed)) await artist(membership.personId);
  const read = [];
  for (const band of bands) read.push(await artist(band.mbid));
  return {
    source: "MusicBrainz web service, https://musicbrainz.org/ws/2/ (core data, CC0)",
    fetched: new Date().toISOString().slice(0, 10),
    relationship: "member of band",
    bands: read.map((band) => ({
      name: band.name,
      mbid: band.id,
      begin: band["life-span"]?.begin ?? null,
      end: band["life-span"]?.end ?? null,
    })),
    memberships: read.flatMap(membershipsOf),
    supplement,
  };
};

// --- Memberships with years ----------------------------------------------

// Every membership that can be placed, with whole years: MusicBrainz's own
// where it has them, else the supplement's, else the band's own span if it
// lasted three years or less. An undated membership with none of these is
// left out.
const datedMemberships = (facts) => {
  const placed = [];
  for (const membership of facts.memberships) {
    const band = facts.bands.find((found) => found.name === membership.band);
    if (membership.begin) {
      placed.push({ ...membership, from: year(membership.begin), to: year(membership.end) ?? Infinity, origin: "musicbrainz" });
      continue;
    }
    const filled = supplement.filter((entry) => entry.band === membership.band && entry.person === membership.person);
    if (filled.length > 0) {
      for (const entry of filled) placed.push({ ...membership, from: year(entry.begin), to: year(entry.end) ?? Infinity, origin: "supplement" });
    } else if (band.begin && band.end && year(band.end) - year(band.begin) <= 3) {
      placed.push({ ...membership, from: year(band.begin), to: year(band.end), origin: "band span" });
    }
  }
  return placed;
};

// --- Line-ups --------------------------------------------------------------
//
// MusicBrainz gives most of these dates as a year and nothing finer, so a
// line-up here is "who was in the band in a year when someone joined or
// left". A member counts in the year they joined, and not in the year they
// left (they left during it, and whoever came in is named for it). Someone
// who joined and left within one year counts in that year.

const member = (membership, at) => membership.from <= at && (at < membership.to || membership.from === at);

const lineUpsOf = (placed, band) => {
  const memberships = placed.filter((membership) => membership.band === band.name);
  const start = Math.max(firstYear, band.from ?? firstYear);
  const changes = [...new Set(memberships.flatMap((membership) => [membership.from, membership.to]))]
    .filter((at) => at >= start && at <= lastYear)
    .sort((one, two) => one - two);
  // A band already going when the drawing starts is drawn from then.
  if (memberships.some((membership) => member(membership, start)) && !changes.includes(start)) changes.unshift(start);
  const lineUps = [];
  for (const at of changes) {
    const members = [...new Set(memberships.filter((membership) => member(membership, at)).map((membership) => membership.person))];
    if (members.length === 0) continue;
    const previous = lineUps[lineUps.length - 1];
    if (previous && previous.members.join() === members.join()) continue;
    lineUps.push({ band: band.name, label: band.label, year: at, members });
  }
  return lineUps;
};

// --- DOT -------------------------------------------------------------------

// A band's colour mixed far towards white, for the inside of its boxes, so
// that black text on it stays easy to read.
const tint = (colour) =>
  "#" +
  [1, 3, 5]
    .map((at) => Math.round(255 - (255 - parseInt(colour.slice(at, at + 2), 16)) * 0.12).toString(16).padStart(2, "0"))
    .join("");

// The bands that have at least one line-up in the drawing, for the legend.
const drawnBands = (facts) => {
  const placed = datedMemberships(facts);
  return bands
    .map((band) => ({ ...band, name: facts.bands.find((found) => found.mbid === band.mbid).name }))
    .filter((band) => lineUpsOf(placed, band).length > 0)
    .map((band) => ({ name: band.label, colour: band.colour }));
};

// A DOT string. `\n` in a label is DOT's own line break and is written
// through as it is.
const quote = (written) => '"' + written.replace(/"/g, '\\"') + '"';

const toDot = (facts) => {
  const placed = datedMemberships(facts);
  const drawn = bands.map((band) => ({ ...band, name: facts.bands.find((found) => found.mbid === band.mbid).name }));
  const lineUps = drawn.flatMap((band) => lineUpsOf(placed, band));
  const id = (lineUp) => `${lineUp.label} ${lineUp.year}`;
  const colourOf = (bandName) => drawn.find((band) => band.name === bandName).colour;
  const years = [...new Set(lineUps.map((lineUp) => lineUp.year))].sort((one, two) => one - two);

  const lines = [];
  lines.push(`// Fairport Convention and the bands around it, ${firstYear} to ${lastYear}.`);
  lines.push(`// Generated by site/scripts/fairport.mjs from MusicBrainz (CC0), fetched ${facts.fetched};`);
  lines.push(`// a few undated memberships filled in by hand (see facts.json, "supplement").`);
  lines.push(`// Each box is a band in a year its line-up changed, with its members then.`);
  lines.push(`// Bold lines carry a band on; thin ones are a person moving to another band.`);
  lines.push(`digraph fairport {`);
  lines.push(`  nodesep=0.25; ranksep=0.3;`);
  lines.push(`  node [shape=box, style="rounded,filled", fontsize=11];`);
  lines.push(`  edge [arrowsize=0.6];`);
  lines.push(``);
  lines.push(`  // The years down the left, one rank each.`);
  lines.push(`  { node [shape=plaintext, style="", fontsize=13];`);
  lines.push(`    ${years.map((at) => quote(String(at))).join(" -> ")} [color="#cccccc", dir=none]; }`);
  lines.push(``);
  for (const at of years) {
    const here = lineUps.filter((lineUp) => lineUp.year === at);
    lines.push(`  { rank=same; ${[quote(String(at)), ...here.map((lineUp) => quote(id(lineUp)))].join("; ")}; }`);
  }
  lines.push(``);
  for (const lineUp of lineUps) {
    // The band's name in bold over its members in smaller type, as an
    // HTML-like label.
    const plain = (text) => text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
    const label = `<<b>${plain(lineUp.label)}</b><br/><font point-size="9">${lineUp.members.map(plain).join("<br/>")}</font>>`;
    const colour = colourOf(lineUp.band);
    lines.push(`  ${quote(id(lineUp))} [label=${label}, color=${quote(colour)}, fillcolor=${quote(tint(colour))}];`);
  }
  lines.push(``);
  lines.push(`  // A band carried on.`);
  for (const band of drawn) {
    const own = lineUps.filter((lineUp) => lineUp.band === band.name);
    for (let at = 1; at < own.length; at++) {
      lines.push(`  ${quote(id(own[at - 1]))} -> ${quote(id(own[at]))} [color=${quote(band.colour)}, style=bold, weight=8];`);
    }
  }
  lines.push(``);
  lines.push(`  // A person joining another band: from the last line-up they were in`);
  lines.push(`  // before, to the first line-up of the band they joined.`);
  const people = [...new Set(lineUps.flatMap((lineUp) => lineUp.members))];
  const moves = new Map();
  for (const person of people) {
    const theirs = lineUps.filter((lineUp) => lineUp.members.includes(person));
    const joinings = placed
      .filter((membership) => membership.person === person && membership.from >= firstYear && membership.from <= lastYear)
      .sort((one, two) => one.from - two.from);
    for (const joining of joinings) {
      const arrival = theirs.find((lineUp) => lineUp.band === joining.band && lineUp.year >= joining.from);
      if (!arrival) continue;
      // The line-up of another band they were last in before arriving.
      const before = theirs
        .filter((lineUp) => lineUp.band !== joining.band && lineUp.year < arrival.year)
        .sort((one, two) => one.year - two.year)
        .pop();
      if (!before) continue;
      const key = `${id(before)}|${id(arrival)}`;
      moves.set(key, [...(moves.get(key) ?? []), person]);
    }
  }
  for (const [key, movers] of moves) {
    const [from, to] = key.split("|");
    lines.push(`  ${quote(from)} -> ${quote(to)} [label=${quote(movers.join("\\n"))}, color="#8a8a8a", fontsize=9];`);
  }
  lines.push(`}`);
  return lines.join("\n") + "\n";
};

// --- Writing -----------------------------------------------------------------

mkdirSync(destination, { recursive: true });
const factsFile = join(destination, "facts.json");
// The supplement is always this script's own, so that it can be corrected
// without asking MusicBrainz again.
const facts = { ...(offline ? JSON.parse(readFileSync(factsFile, "utf8")) : await gather()), supplement };
writeFileSync(factsFile, JSON.stringify(facts, null, 2) + "\n");
writeFileSync(join(destination, "fairport.gv"), toDot(facts));
writeFileSync(join(destination, "bands.json"), JSON.stringify(drawnBands(facts), null, 2) + "\n");
const placed = datedMemberships(facts);
const count = bands
  .map((band) => ({ ...band, name: facts.bands.find((found) => found.mbid === band.mbid).name }))
  .reduce((total, band) => total + lineUpsOf(placed, band).length, 0);
console.log(`${facts.memberships.length} memberships read, ${placed.length} placed, ${count} line-ups; fetched ${facts.fetched}`);
