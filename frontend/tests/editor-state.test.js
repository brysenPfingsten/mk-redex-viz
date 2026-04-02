import test from "node:test";
import assert from "node:assert/strict";

import {
  deriveEditableCodeState,
  deriveFrozenEditorState,
  deriveLoadedExampleState,
  deriveThawedEditorState,
} from "../src/utils/editor_state.js";

test("deriveFrozenEditorState stores the editable source snapshot and shows tagged code", () => {
  assert.deepEqual(
    deriveFrozenEditorState("(run* (q) (== q 'cat))", "[[u0]](run* (q) (== q 'cat))[[/u0]]"),
    {
      code: "[[u0]](run* (q) (== q 'cat))[[/u0]]",
      originalCode: "(run* (q) (== q 'cat))",
      initialTaggedCode: "[[u0]](run* (q) (== q 'cat))[[/u0]]",
      isFrozen: true,
      isAtStart: true,
      isAtEnd: false,
    },
  );
});

test("deriveThawedEditorState restores the editable source view at semantic start", () => {
  assert.deepEqual(
    deriveThawedEditorState("(run* (q) (== q 'cat))"),
    {
      code: "(run* (q) (== q 'cat))",
      isFrozen: false,
      isAtStart: true,
      isAtEnd: false,
    },
  );
});

test("deriveLoadedExampleState records the example source of truth", () => {
  assert.deepEqual(
    deriveLoadedExampleState("same", "(run* (q) (== q 'same))"),
    {
      code: "(run* (q) (== q 'same))",
      selectedExampleId: "same",
      selectedExampleSource: "(run* (q) (== q 'same))",
    },
  );
});

test("deriveEditableCodeState clears the example selection when the buffer diverges", () => {
  assert.deepEqual(
    deriveEditableCodeState(
      "(run* (q) (== q 'dog))",
      "same",
      "(run* (q) (== q 'cat))",
    ),
    {
      code: "(run* (q) (== q 'dog))",
      selectedExampleId: "",
      selectedExampleSource: "",
    },
  );
});

test("deriveEditableCodeState preserves the example selection while the buffer still matches", () => {
  assert.deepEqual(
    deriveEditableCodeState(
      "(run* (q) (== q 'cat))",
      "same",
      "(run* (q) (== q 'cat))",
    ),
    {
      code: "(run* (q) (== q 'cat))",
      selectedExampleId: "same",
      selectedExampleSource: "(run* (q) (== q 'cat))",
    },
  );
});
