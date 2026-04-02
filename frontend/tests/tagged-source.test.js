import test from "node:test";
import assert from "node:assert/strict";

import { parseTaggedText } from "../src/utils/tagged_source.js";

test("parseTaggedText strips markers and keeps source segments for repeated ids", () => {
  const tagged = "[[d0]](conde [[u1]](== q 'four)[[/u1]] [[u1]](== q 'five)[[/u1]])[[/d0]]";
  const { plain, segments } = parseTaggedText(tagged);

  assert.equal(plain, "(conde (== q 'four) (== q 'five))");
  assert.deepEqual(
    segments.filter(({ id }) => id === "u1").map(({ start, end }) => [start, end]),
    [
      [7, 19],
      [20, 32],
    ],
  );
  assert.deepEqual(
    segments.filter(({ id }) => id === "d0").map(({ start, end }) => [start, end]),
    [[0, plain.length]],
  );
});

test("parseTaggedText accepts hyphenated ids", () => {
  const tagged = "[[fresh-0]](fresh (x) ...)[[/fresh-0]]";
  const { plain, segments } = parseTaggedText(tagged);

  assert.equal(plain, "(fresh (x) ...)");
  assert.deepEqual(segments, [{ id: "fresh-0", start: 0, end: plain.length }]);
});
