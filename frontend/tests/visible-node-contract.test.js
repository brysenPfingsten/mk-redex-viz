import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { DRAWABLE_NODE_NAMES } from "../src/utils/drawing.js";
import { addColors } from "../src/utils/treeSetup.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const CONTRACT_PATH = path.resolve(__dirname, "../../contracts/visible-node-contract.json");

function readContract() {
  return JSON.parse(fs.readFileSync(CONTRACT_PATH, "utf8"));
}

test("frontend renderer covers every visible node kind in the shared contract", () => {
  const { visibleNodeNames } = readContract();
  assert.deepEqual(
    [...DRAWABLE_NODE_NAMES].sort(),
    [...visibleNodeNames].sort(),
  );
});

test("frontend active-path logic follows explicit backend metadata instead of node-name tables", () => {
  const tree = {
    name: "Opaque-Wrapper",
    activeChildIndex: 1,
    children: [
      {
        name: "Opaque-Resolved",
        nodeColor: "green",
      },
      {
        name: "Opaque-Branch",
        focusColor: "#ff8000",
        activeChildIndex: 0,
        children: [
          { name: "Opaque-Leaf" },
        ],
      },
    ],
  };

  const result = addColors(tree);
  assert.equal(result.color, "#ff8000");
  assert.equal(result.children[1].color, "#ff8000");
  assert.equal(result.children[1].children[0].color, "#ff8000");
});
