import test from "node:test";
import assert from "node:assert/strict";

import {
  goalIdFromTreeNodeData,
  selectedSourceSegments,
  treeNodesWithGoalId,
} from "../src/utils/source_mapping.js";

test("selectedSourceSegments returns every source span sharing the selected UUID", () => {
  const segments = [
    { id: "u-1", start: 0, end: 4 },
    { id: "u-2", start: 5, end: 9 },
    { id: "u-1", start: 10, end: 14 },
  ];

  assert.deepEqual(selectedSourceSegments(segments, "u-1"), [
    { id: "u-1", start: 0, end: 4 },
    { id: "u-1", start: 10, end: 14 },
  ]);
  assert.deepEqual(selectedSourceSegments(segments, null), []);
});

test("goalIdFromTreeNodeData uses the tree node UUID and preserves fallback for state-only nodes", () => {
  assert.equal(goalIdFromTreeNodeData({ id: "eq-1", stateId: "st-1" }, "old-id"), "eq-1");
  assert.equal(goalIdFromTreeNodeData({ stateId: "st-1" }, "old-id"), "old-id");
  assert.equal(goalIdFromTreeNodeData({ stateId: "st-1" }, null), null);
});

test("treeNodesWithGoalId finds all RHS tree nodes that share a source UUID", () => {
  const tree = {
    name: "Goal-Disj",
    children: [
      {
        name: "Unify",
        id: "eq-1",
      },
      {
        name: "Conjunction",
        children: [
          {
            name: "Rel-Call",
            id: "rel-1",
          },
          {
            name: "Unify",
            id: "eq-1",
          },
        ],
      },
    ],
  };

  const matches = treeNodesWithGoalId(tree, "eq-1");
  assert.equal(matches.length, 2);
  assert.deepEqual(matches.map((node) => node.name), ["Unify", "Unify"]);
  assert.deepEqual(treeNodesWithGoalId(tree, "missing"), []);
});
