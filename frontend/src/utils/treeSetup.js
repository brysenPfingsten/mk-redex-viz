function validIndex(children, idx) {
  return Number.isInteger(idx)
    && Array.isArray(children)
    && idx >= 0
    && idx < children.length;
}

function activeChild(node) {
  const idx = node?.activeChildIndex;
  return validIndex(node?.children, idx) ? node.children[idx] : null;
}

function resolvedChildIndices(node) {
  if (!Array.isArray(node?.resolvedChildIndices)) return [];
  return node.resolvedChildIndices.filter((idx) => validIndex(node?.children, idx));
}

function nodeFocusColor(node) {
  if (!node) return null;
  if (typeof node.focusColor === "string") return node.focusColor;
  return nodeFocusColor(activeChild(node));
}

function paintResolved(node, color) {
  if (!node || typeof color !== "string") return;
  node.color = color;
  node.edgeColor = color;
  if (Array.isArray(node.children)) {
    for (const child of node.children) {
      paintResolved(child, color);
    }
  }
}

export const ACTIVE_PATH_NODE_NAMES = Object.freeze([]);

export function addColors(tree) {
  if (!tree || tree.partial) return tree;

  if (typeof tree.nodeColor === "string") {
    tree.color = tree.nodeColor;
  }

  for (const idx of resolvedChildIndices(tree)) {
    paintResolved(tree.children[idx], tree.resolvedColor ?? tree.nodeColor ?? "green");
  }

  const active = activeChild(tree);
  if (!active) {
    return tree;
  }

  const color = tree.color ?? nodeFocusColor(tree);
  if (typeof color !== "string") {
    return tree;
  }

  tree.color = color;
  active.edgeColor = color;
  if (typeof active.color !== "string") {
    active.color = color;
  }
  addColors(active);

  return tree;
}
