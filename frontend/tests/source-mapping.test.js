import test from "node:test";
import assert from "node:assert/strict";
import { parseTaggedText } from "../src/utils/tagged_source.js";

import {
  goalIdFromTreeNodeData,
  stateKeyFromTreeNodeData,
  selectedSourceSegments,
  treeNodesWithGoalId,
} from "../src/utils/source_mapping.js";

test("state inspection uses structural identity when distinct states retain the same semantic tag", () => {
  const left = { stateId: "initial", stateKey: "(world state-a)" };
  const right = { stateId: "initial", stateKey: "(world state-b)" };
  assert.notEqual(stateKeyFromTreeNodeData(left), stateKeyFromTreeNodeData(right));
  assert.equal(stateKeyFromTreeNodeData({ stateId: "old" }), "old");
  assert.equal(stateKeyFromTreeNodeData(null), null);
});

test("a terminal Solo directly exposes its state and each introduction source", () => {
  const solo = {
    name: "Solo", semanticKind: "frontier", renderRole: "terminal-answer", children: [],
    stateId: "terminal", stateKey: "(scope terminal-state)",
    sub: [{ key: 9, value: { sym: "ready" } }], reified: { sym: "ready" },
    owners: [{ vars: [], sourceId: "unused" }, { vars: [9, 2], sourceId: "query" }],
  };
  assert.equal(stateKeyFromTreeNodeData(solo), "(scope terminal-state)");
  assert.deepEqual(treeNodesWithGoalId(solo, "unused"), [solo]);
  assert.deepEqual(treeNodesWithGoalId(solo, "query"), [solo]);
  assert.deepEqual(solo.children, []);
  assert.deepEqual(solo.sub, [{ key: 9, value: { sym: "ready" } }]);
});

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

test("goalIdFromTreeNodeData only returns a source UUID when the tree node actually has one", () => {
  assert.equal(goalIdFromTreeNodeData({ id: "eq-1", stateId: "st-1" }), "eq-1");
  assert.equal(goalIdFromTreeNodeData({ stateId: "st-1" }), null);
  assert.equal(goalIdFromTreeNodeData(null), null);
  assert.equal(goalIdFromTreeNodeData({ id: "hidden:query" }), null);
  const owners = [{ vars: [{ var: "u:0" }], sourceId: "fresh-1" }];
  assert.equal(goalIdFromTreeNodeData({ owners, stateId: "st-1" }), null);
  assert.equal(goalIdFromTreeNodeData({ id: "eq-1", owners, stateId: "st-1" }), "eq-1");
});

test("source selection finds exact owning nodes without merging common and answer-private introductions", () => {
  const common = [
    { vars: [{ var: "u:0" }, { var: "u:1" }], sourceId: "fresh-common" },
    { vars: [], sourceId: "fresh-empty" },
  ];
  const privateOwners = [{ vars: [{ var: "u:2" }], sourceId: "fresh-private" }];
  const answer = { name: "Answer", stateId: "st-1", owners: privateOwners };
  const delayed = {
    name: "Delay",
    owners: [{ vars: [{ var: "u:2" }], sourceId: "fresh-residual" }],
  };
  const tree = {
    name: "Emit", owners: common,
    children: [answer, { name: "More", children: [delayed] }],
  };
  const { segments } = parseTaggedText(
    "[[fresh-common]](fresh (x y) [[fresh-empty]](fresh () "
    + "(disj [[fresh-private]](fresh (private) succeed)[[/fresh-private]] "
    + "[[fresh-residual]](fresh (residual) (Zzz succeed))[[/fresh-residual]]))"
    + "[[/fresh-empty]])[[/fresh-common]]",
  );

  for (const [id, owner] of [
    ["fresh-common", tree], ["fresh-empty", tree],
    ["fresh-private", answer], ["fresh-residual", delayed],
  ]) {
    assert.equal(selectedSourceSegments(segments, id).length, 1);
    assert.deepEqual(treeNodesWithGoalId(tree, id), [owner]);
  }
  assert.equal(goalIdFromTreeNodeData(answer), null);
  assert.equal(stateKeyFromTreeNodeData(answer), "st-1");
  assert.deepEqual(treeNodesWithGoalId(tree, "st-1"), []);
  assert.equal(tree.owners, common);
  assert.equal(answer.owners, privateOwners);
  assert.equal(common.length, 2);
  assert.equal(common[0].vars.length, 2);
  assert.equal(common[1].vars.length, 0);
});

test("owner source matching preserves repeated origins without duplicating a node or selecting hidden owners", () => {
  const visibleOwner = { vars: [], sourceId: "fresh-1" };
  const hiddenOwner = { vars: [{ var: "u:0" }], sourceId: "hidden:fresh-1" };
  const ownOrigin = { name: "Eval", id: "fresh-1", owners: [visibleOwner, visibleOwner] };
  const sharedOrigin = { name: "Forced", owners: [visibleOwner] };
  const hiddenOrigin = { name: "Solo", owners: [hiddenOwner] };
  const tree = { name: "Mplus", children: [ownOrigin, sharedOrigin, hiddenOrigin] };

  assert.deepEqual(treeNodesWithGoalId(tree, "fresh-1"), [ownOrigin, sharedOrigin]);
  assert.deepEqual(treeNodesWithGoalId(tree, "hidden:fresh-1"), []);
  assert.equal(goalIdFromTreeNodeData(hiddenOrigin), null);
  assert.deepEqual(hiddenOrigin.owners, [hiddenOwner]);
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

test("nested conde compiler markup selects the correct leaf and shared compound origin", () => {
  // Actual default-profile compiler output for the same example. Source IDs
  // are assigned before lowering: the two binary inner choices share d3.
  const markup = `(defrel (same x y)
  [[u0]](== x y)[[/u0]])

[[f1]](run* (q) [[d2]](conde
  [[[d3]](conde
    [[[r4]](same q 'turtle)[[/r4]]]
    [[[r5]](same q 'cat)[[/r5]]]
    [[[u6]](== q 'dog)[[/u6]]]
  )[[/d3]]]
  [[[r7]](same q 'fish)[[/r7]]]
)[[/d2]])[[/f1]]`;
  const { plain, segments } = parseTaggedText(markup);
  for (const [id, expected] of [["r4", "(same q 'turtle)"], ["r5", "(same q 'cat)"], ["u6", "(== q 'dog)"], ["r7", "(same q 'fish)"]]) {
    const selected = selectedSourceSegments(segments, goalIdFromTreeNodeData({ id }));
    assert.equal(selected.length, 1);
    assert.equal(plain.slice(selected[0].start, selected[0].end), expected);
  }
  const choices = { id: "d3", children: [{ id: "r4" }, { id: "d3", children: [{ id: "r5" }, { id: "u6" }] }] };
  assert.equal(treeNodesWithGoalId(choices, "d3").length, 2);
  const [origin] = selectedSourceSegments(segments, "d3");
  for (const id of ["r4", "r5", "u6"]) {
    const [leaf] = selectedSourceSegments(segments, id);
    assert.ok(origin.start <= leaf.start && leaf.end <= origin.end);
  }
});
