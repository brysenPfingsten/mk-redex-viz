export function selectedSourceSegments(segments, goalId) {
  if (goalId == null) return [];
  return segments.filter((segment) => segment.id === goalId);
}

export function goalIdFromTreeNodeData(data) {
  const id = data?.id;
  return typeof id === "string" && !id.startsWith("hidden:") ? id : null;
}

export function stateKeyFromTreeNodeData(data) {
  return data?.stateKey ?? data?.stateId ?? null;
}

export function treeNodesWithGoalId(node, goalId, acc = []) {
  if (typeof goalId !== "string" || goalId.startsWith("hidden:") || node == null) return acc;
  if (Array.isArray(node)) {
    for (const child of node) {
      treeNodesWithGoalId(child, goalId, acc);
    }
    return acc;
  }
  if (typeof node !== "object") return acc;
  if (node.id === goalId || (Array.isArray(node.owners)
    && node.owners.some(owner => owner.sourceId === goalId))) {
    acc.push(node);
  }
  if (Array.isArray(node.children)) {
    for (const child of node.children) {
      treeNodesWithGoalId(child, goalId, acc);
    }
  }
  return acc;
}
