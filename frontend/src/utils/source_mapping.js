export function selectedSourceSegments(segments, goalId) {
  if (goalId == null) return [];
  return segments.filter((segment) => segment.id === goalId);
}

export function goalIdFromTreeNodeData(data, fallbackGoalId = null) {
  return data?.id ?? fallbackGoalId ?? null;
}

export function treeNodesWithGoalId(node, goalId, acc = []) {
  if (goalId == null || node == null) return acc;
  if (Array.isArray(node)) {
    for (const child of node) {
      treeNodesWithGoalId(child, goalId, acc);
    }
    return acc;
  }
  if (typeof node !== "object") return acc;
  if (node.id === goalId) {
    acc.push(node);
  }
  if (Array.isArray(node.children)) {
    for (const child of node.children) {
      treeNodesWithGoalId(child, goalId, acc);
    }
  }
  return acc;
}
