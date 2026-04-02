export function deriveFrozenEditorState(sourceCode, taggedCode) {
  const visibleCode = taggedCode || sourceCode;
  return {
    code: visibleCode,
    originalCode: sourceCode,
    initialTaggedCode: visibleCode,
    isFrozen: true,
    isAtStart: true,
    isAtEnd: false,
  };
}

export function deriveThawedEditorState(sourceCode) {
  return {
    code: sourceCode,
    isFrozen: false,
    isAtStart: true,
    isAtEnd: false,
  };
}

export function deriveLoadedExampleState(exampleId, sourceCode) {
  return {
    code: sourceCode,
    selectedExampleId: exampleId,
    selectedExampleSource: sourceCode,
  };
}

export function deriveEditableCodeState(nextCode, selectedExampleId, selectedExampleSource) {
  if (!selectedExampleId || nextCode === selectedExampleSource) {
    return {
      code: nextCode,
      selectedExampleId,
      selectedExampleSource,
    };
  }

  return {
    code: nextCode,
    selectedExampleId: "",
    selectedExampleSource: "",
  };
}
