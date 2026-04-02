import test from "node:test";
import assert from "node:assert/strict";

import { addColors } from "../src/utils/treeSetup.js";

test("addColors preserves binary Goal-Conj nesting", () => {
  const tree = {
    name: "Goal-Conj",
    children: [
      {
        name: "Goal-Conj",
        children: [
          { name: "Goal-Delay", children: [{ name: "Rel-Call" }] },
          { name: "Unify" },
        ],
      },
      { name: "Unify" },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.name, "Goal-Conj");
  assert.equal(result.children.length, 2);
  assert.equal(result.children[0].name, "Goal-Conj");
  assert.equal(result.children[0].children.length, 2);
});
