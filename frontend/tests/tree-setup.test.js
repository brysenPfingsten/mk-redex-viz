import test from "node:test";
import assert from "node:assert/strict";

import { addColors } from "../src/utils/treeSetup.js";

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

test("addColors carries spine color through freshened nodes", () => {
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
