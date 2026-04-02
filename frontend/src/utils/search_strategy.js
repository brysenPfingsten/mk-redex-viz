export const DEFAULT_SEARCH_STRATEGY = Object.freeze({
  hoist: "early",
  scheduler: "rail",
});

export const HOIST_OPTIONS = Object.freeze([
  Object.freeze({ value: "early", label: "Early" }),
  Object.freeze({ value: "late", label: "Late" }),
]);

export const SCHEDULER_OPTIONS = Object.freeze([
  Object.freeze({ value: "dfs", label: "No Interleave" }),
  Object.freeze({ value: "flip", label: "Flip-Flop" }),
  Object.freeze({ value: "rail", label: "Railroad" }),
]);
