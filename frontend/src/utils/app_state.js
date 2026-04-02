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
    canReset: isFrozen && !isAtStart,
    canBack: isFrozen && !isAtStart,
    canStep: isFrozen && !isAtEnd,
  };
}
