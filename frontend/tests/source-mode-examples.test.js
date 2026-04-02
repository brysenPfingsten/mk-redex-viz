import test from "node:test";
import assert from "node:assert/strict";

import { exampleById, exampleOptions } from "../src/utils/example_programs.js";
import { DEFAULT_MODEL_OPTIONS, MODEL_IDS } from "../src/utils/model_ids.js";
import {
  buildInitOptions,
  buildSourceOptions,
  CONJ_ASSOC_OPTIONS,
  DELAY_PLACEMENT_OPTIONS,
  DEFAULT_COMPILE_PROFILE,
  DISJ_ASSOC_OPTIONS,
  SOURCE_MODE_OPTIONS,
} from "../src/utils/source_defaults.js";

test("buildSourceOptions includes compileProfile for mini source", () => {
  assert.deepEqual(
    buildSourceOptions("(run* (q) (== q 'cat))", "mini", DEFAULT_COMPILE_PROFILE),
    {
      text: "(run* (q) (== q 'cat))",
      sourceMode: "mini",
      compileProfile: DEFAULT_COMPILE_PROFILE,
    },
  );
});

test("buildSourceOptions omits compileProfile for micro source", () => {
  assert.deepEqual(
    buildSourceOptions("(run* (q) (== q 'cat))", "micro", DEFAULT_COMPILE_PROFILE),
    {
      text: "(run* (q) (== q 'cat))",
      sourceMode: "micro",
    },
  );
});

test("buildInitOptions carries the selected model id", () => {
  assert.deepEqual(
    buildInitOptions(
      "(run* (q) (== q 'cat))",
      "mini",
      DEFAULT_COMPILE_PROFILE,
      MODEL_IDS.L3_DFS_LAZY,
    ),
    {
      text: "(run* (q) (== q 'cat))",
      sourceMode: "mini",
      compileProfile: DEFAULT_COMPILE_PROFILE,
      model: MODEL_IDS.L3_DFS_LAZY,
    },
  );
});

test("exampleOptions exposes stable ids instead of raw program text", () => {
  const options = exampleOptions();
  assert.equal(options[0].value, "");
  assert.equal(options[0].label, "Examples");
  assert.ok(options.some((opt) => opt.value === "appendoh-1"));
  assert.ok(options.every((opt) => typeof opt.value === "string"));
});

test("exampleById returns the semantic example source of truth", () => {
  const example = exampleById("same");
  assert.equal(example.label, "same");
  assert.match(example.miniSource, /defrel/);
  assert.equal(exampleById("missing-example"), null);
});

test("default model option labels no longer prefix every entry with µKanren", () => {
  assert.ok(DEFAULT_MODEL_OPTIONS.length > 0);
  assert.ok(DEFAULT_MODEL_OPTIONS.every(({ label }) => !label.includes("µKanren")));
  assert.deepEqual(
    DEFAULT_MODEL_OPTIONS.map(({ value }) => value),
    [
      MODEL_IDS.L3_DFS_LAZY,
      MODEL_IDS.L3_FLIP_LAZY,
      MODEL_IDS.L4_RAIL_LAZY,
      MODEL_IDS.L3_DFS_EAGER,
      MODEL_IDS.L3_FLIP_EAGER,
      MODEL_IDS.L4_RAIL_EAGER,
    ],
  );
});

test("source mode and compile profile option catalogs expose the expected axes", () => {
  assert.deepEqual(
    SOURCE_MODE_OPTIONS.map(({ value }) => value),
    ["mini", "micro"],
  );
  assert.deepEqual(
    CONJ_ASSOC_OPTIONS.map(({ value }) => value),
    ["left", "right"],
  );
  assert.deepEqual(
    DISJ_ASSOC_OPTIONS.map(({ value }) => value),
    ["left", "right"],
  );
  assert.deepEqual(
    DELAY_PLACEMENT_OPTIONS.map(({ value }) => value),
    ["relbody", "relcall", "disj"],
  );
});
