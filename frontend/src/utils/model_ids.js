export const MODEL_IDS = Object.freeze({
  L0_CORE: "mk-l0-core",
  L1_CALL_LAZY: "mk-l1-call-lazy",
  L1_CALL_EAGER: "mk-l1-call-eager",
  L2_DISJ_LEFT: "mk-l2-disj-left",
  L4_RAIL_LAZY: "mk-l4-rail-lazy",
  L3_DFS_LAZY: "mk-l3-dfs-lazy",
  L3_FLIP_LAZY: "mk-l3-flip-lazy",
  L4_RAIL_EAGER: "mk-l4-rail-eager",
  L3_DFS_EAGER: "mk-l3-dfs-eager",
  L3_FLIP_EAGER: "mk-l3-flip-eager",
});

export const DEFAULT_MODEL_OPTIONS = Object.freeze([
  Object.freeze({
    value: MODEL_IDS.L3_DFS_LAZY,
    label: "(No Interleave, Lazy)",
  }),
  Object.freeze({
    value: MODEL_IDS.L3_FLIP_LAZY,
    label: "(Interleave + Flip-Flop, Lazy)",
  }),
  Object.freeze({
    value: MODEL_IDS.L4_RAIL_LAZY,
    label: "(Interleave + Railroad, Lazy)",
  }),
  Object.freeze({
    value: MODEL_IDS.L3_DFS_EAGER,
    label: "(No Interleave, Eager)",
  }),
  Object.freeze({
    value: MODEL_IDS.L3_FLIP_EAGER,
    label: "(Interleave + Flip-Flop, Eager)",
  }),
  Object.freeze({
    value: MODEL_IDS.L4_RAIL_EAGER,
    label: "(Interleave + Railroad, Eager)",
  }),
]);
