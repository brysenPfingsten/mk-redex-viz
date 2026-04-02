export function deriveToolbarState({
  isFrozen,
  code,
  isExampleLoading,
  isAtStart,
  isAtEnd,
}) {
  const hasRunnableCode = code.trim() !== "";

  return {
    canStart: !isFrozen && hasRunnableCode && !isExampleLoading,
    canReset: isFrozen,
    canBack: isFrozen && !isAtStart,
    canStep: isFrozen && !isAtEnd,
  };
}

export function nextSelectedExampleId({
  selectedExampleId,
  selectedExampleSource,
  nextCode,
}) {
  if (!selectedExampleId) {
    return "";
  }
  return nextCode === selectedExampleSource ? selectedExampleId : "";
}
