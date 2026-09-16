import test from "node:test";
import assert from "node:assert/strict";

import { addColors } from "../src/utils/treeSetup.js";

test("strict maturation highlights the right operand without resolving the left candidate as an answer", () => {
  const tree = {
    name: "Mplus", focusColor: "#ff8000", activeChildIndex: 1,
    children: [
      { name: "One", children: [{ name: "Candidate", nodeColor: "#fff2cc" }] },
      { name: "Eval", activeChildIndex: 0, children: [{ name: "Unify" }] },
    ],
  };
  const result = addColors(tree);
  assert.equal(result.children[0].edgeColor, undefined);
  assert.equal(result.children[0].children[0].nodeColor, "#fff2cc");
  assert.equal(result.children[1].edgeColor, "#ff8000");
});

test("a paused strict Frontier does not highlight work inside its Delay", () => {
  const tree = {
    name: "More", children: [{ name: "Delay", suspended: true, children: [
      { name: "Bind", focusColor: "blue", activeChildIndex: 0, children: [{ name: "Eval" }] },
    ] }],
  };
  const result = addColors(tree);
  assert.equal(result.children[0].color, undefined);
  assert.equal(result.children[0].children[0].children[0].edgeColor, undefined);
});

test("addColors preserves binary Goal-Conj nesting", () => {
  const tree = {
    name: "Goal-Conj",
    focusColor: "blue",
    activeChildIndex: 0,
    children: [
      {
        name: "Goal-Conj",
        focusColor: "blue",
        activeChildIndex: 0,
        children: [
          {
            name: "Goal-Delay",
            activeChildIndex: 0,
            children: [{ name: "Rel-Call" }],
          },
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

test("addColors preserves the search-tree color through an answer prefix", () => {
  const tree = {
    name: "Emit",
    resolvedChildIndices: [0],
    resolvedColor: "green",
    activeChildIndex: 1,
    children: [
      { name: "Answer", nodeColor: "green" },
      {
        name: "<-+",
        focusColor: "#ff8000",
        activeChildIndex: 0,
        children: [
          { name: "Answer", nodeColor: "green" },
          { name: "Unify" },
        ],
      },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.children[0].edgeColor, "green");
  assert.equal(result.children[1].edgeColor, "#ff8000");
});

test("addColors keeps the active edge colored when a disjunction points at an answer", () => {
  const tree = {
    name: "<-+",
    focusColor: "#ff8000",
    activeChildIndex: 0,
    children: [
      { name: "Answer", nodeColor: "green" },
      { name: "Unify" },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].color, "green");
});

test("addColors keeps the active edge colored through a spine prefix to an answer", () => {
  const tree = {
    name: "Freshened",
    activeChildIndex: 0,
    children: [
      {
        name: "Emit",
        resolvedChildIndices: [0],
        resolvedColor: "green",
        activeChildIndex: 1,
        children: [
          { name: "Answer", nodeColor: "green" },
          {
            name: "<-+",
            focusColor: "#ff8000",
            activeChildIndex: 0,
            children: [
              { name: "Answer", nodeColor: "green" },
              { name: "Unify" },
            ],
          },
        ],
      },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.color, "#ff8000");
  assert.equal(result.children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[0].edgeColor, "green");
  assert.equal(result.children[0].children[1].edgeColor, "#ff8000");
});

test("addColors follows a strict Frontier without turning owner groups into control nodes", () => {
  const common = [{ vars: [{ var: "u:0" }], sourceId: "fresh-common" }];
  const privateOwners = [{ vars: [{ var: "u:1" }], sourceId: "fresh-private" }];
  const resumed = [{ vars: [], sourceId: "fresh-empty" }];
  const tree = {
    name: "Emit", semanticKind: "frontier", owners: common,
    resolvedChildIndices: [0], resolvedColor: "green", activeChildIndex: 1,
    children: [
      { name: "Answer", semanticKind: "answer", nodeColor: "green", owners: privateOwners },
      {
        name: "Forced", semanticKind: "frontier", owners: resumed, activeChildIndex: 0,
        children: [
          {
            name: "Commit", semanticKind: "operation", focusColor: "blue", activeChildIndex: 0,
            children: [
              { name: "Eval", semanticKind: "operation", activeChildIndex: 0,
                children: [{ name: "Unify", semanticKind: "goal" }] },
            ],
          },
        ],
      },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.color, "blue");
  assert.equal(result.children[0].edgeColor, "green");
  assert.equal(result.children[1].edgeColor, "blue");
  assert.equal(result.children[1].children[0].children[0].children[0].edgeColor, "blue");
  assert.equal(result.owners, common);
  assert.equal(result.children[0].owners, privateOwners);
  assert.equal(result.children[1].owners, resumed);
  assert.equal(result.children.length, 2);
});

test("addColors keeps the active edge colored through nested rail disjunctions", () => {
  const tree = {
    name: "Freshened",
    activeChildIndex: 0,
    children: [
      {
        name: "Emit",
        resolvedChildIndices: [0],
        resolvedColor: "green",
        activeChildIndex: 1,
        children: [
          {
            name: "Answer",
            nodeColor: "green",
          },
          {
            name: "<-+",
            focusColor: "#ff8000",
            activeChildIndex: 0,
            children: [
              {
                name: "+->",
                focusColor: "#ff8000",
                activeChildIndex: 1,
                children: [
                  {
                    name: "Goal-Disj",
                    focusColor: "#ff8000",
                    activeChildIndex: 0,
                    children: [{ name: "Rel-Call" }, { name: "Unify" }],
                  },
                  { name: "Answer", nodeColor: "green" },
                ],
              },
              {
                name: "Goal-Disj",
                focusColor: "#ff8000",
                activeChildIndex: 0,
                children: [{ name: "Rel-Call" }, { name: "Unify" }],
              },
            ],
          },
        ],
      },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.color, "#ff8000");
  assert.equal(result.children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[0].edgeColor, "green");
  assert.equal(result.children[0].children[1].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[1].children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[1].children[0].children[1].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[1].children[0].children[1].color, "green");
});

test("addColors carries the active path through delay nodes", () => {
  const tree = {
    name: "Freshened",
    activeChildIndex: 0,
    children: [
      {
        name: "Deferred",
        activeChildIndex: 0,
        children: [
          {
            name: "Emit",
            resolvedChildIndices: [0],
            resolvedColor: "green",
            activeChildIndex: 1,
            children: [
              { name: "Answer", nodeColor: "green" },
              {
                name: "Delay",
                activeChildIndex: 0,
                children: [
                  {
                    name: "+->",
                    focusColor: "#ff8000",
                    activeChildIndex: 1,
                    children: [
                      {
                        name: "Goal-Disj",
                        focusColor: "#ff8000",
                        activeChildIndex: 0,
                        children: [{ name: "Rel-Call" }, { name: "Unify" }],
                      },
                      {
                        name: "Goal-Disj",
                        focusColor: "#ff8000",
                        activeChildIndex: 0,
                        children: [{ name: "Rel-Call" }, { name: "Unify" }],
                      },
                    ],
                  },
                ],
              },
            ],
          },
        ],
      },
    ],
  };

  const result = addColors(tree);

  assert.equal(result.color, "#ff8000");
  assert.equal(result.children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[0].children[0].edgeColor, "green");
  assert.equal(result.children[0].children[0].children[1].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[0].children[1].children[0].edgeColor, "#ff8000");
  assert.equal(result.children[0].children[0].children[1].children[0].children[1].edgeColor, "#ff8000");
});
