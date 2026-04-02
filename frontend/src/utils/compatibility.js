export function analysisStatusForModel(analysis, modelId) {
  if (!analysis?.validSyntax) return "syntax-error";
  const compatibleIds = analysis.compatibleModelIds || [];
  return compatibleIds.includes(modelId) ? "ok" : "incompatible";
}

export function isStartBlockedByAnalysis({ isFrozen, code, analysisStatus }) {
  if (isFrozen) return false;
  if (typeof code !== "string" || code.trim() === "") return true;
  return analysisStatus === "syntax-error"
    || analysisStatus === "incompatible";
}
