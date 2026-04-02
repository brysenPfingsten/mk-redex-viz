import {
  deriveToolbarState,
} from "../src/utils/app_state.js";
import {
  deriveEditableCodeState,
  deriveFrozenEditorState,
  deriveLoadedExampleState,
  deriveThawedEditorState,
} from "../src/utils/editor_state.js";

const adHocSource = "(run* (q) (== q 'cat))";
const taggedSource = "[[u0]](run* (q) (== q 'cat))[[/u0]]";
const exampleSource = "(run* (q) (== q 'dog))";
const editedExampleSource = "(run* (q) (== q 'bird))";

const frozen = deriveFrozenEditorState(adHocSource, taggedSource);
const thawed = deriveThawedEditorState(adHocSource);
const loadedExample = deriveLoadedExampleState("same", exampleSource);
const divergedExample = deriveEditableCodeState(
  editedExampleSource,
  loadedExample.selectedExampleId,
  loadedExample.selectedExampleSource,
);

const report = {
  scenarios: [
    {
      name: "freeze-init",
      originalCode: frozen.originalCode,
      visibleCode: frozen.code,
      isFrozen: frozen.isFrozen,
    },
    {
      name: "reset-thaws-editor",
      code: thawed.code,
      isFrozen: thawed.isFrozen,
      toolbar: deriveToolbarState({
        isFrozen: thawed.isFrozen,
        code: thawed.code,
        isExampleLoading: false,
        isAtStart: thawed.isAtStart,
        isAtEnd: thawed.isAtEnd,
      }),
    },
    {
      name: "example-edit-clears-selection",
      selectedExampleIdBefore: loadedExample.selectedExampleId,
      selectedExampleIdAfter: divergedExample.selectedExampleId,
      selectedExampleSourceAfter: divergedExample.selectedExampleSource,
      codeAfter: divergedExample.code,
    },
  ],
};

console.log(JSON.stringify(report, null, 2));
