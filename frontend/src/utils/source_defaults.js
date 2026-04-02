export const DEFAULT_SOURCE_MODE = "mini";

export const DEFAULT_COMPILE_PROFILE = Object.freeze({
  conjAssoc: "left",
  disjAssoc: "right",
  delayPlacement: "relbody",
});

export const SOURCE_MODE_OPTIONS = Object.freeze([
  Object.freeze({ value: "mini", label: "miniKanren" }),
  Object.freeze({ value: "micro", label: "microKanren" }),
]);

export const CONJ_ASSOC_OPTIONS = Object.freeze([
  Object.freeze({ value: "left", label: "Left" }),
  Object.freeze({ value: "right", label: "Right" }),
]);

export const DISJ_ASSOC_OPTIONS = Object.freeze([
  Object.freeze({ value: "left", label: "Left" }),
  Object.freeze({ value: "right", label: "Right" }),
]);

export const DELAY_PLACEMENT_OPTIONS = Object.freeze([
  Object.freeze({ value: "relbody", label: "Top of Body" }),
  Object.freeze({ value: "relcall", label: "Every RelCall" }),
  Object.freeze({ value: "disj", label: "Every Disj" }),
]);

export function buildSourceOptions(
  text,
  sourceMode = DEFAULT_SOURCE_MODE,
  compileProfile = DEFAULT_COMPILE_PROFILE,
) {
  return sourceMode === "mini"
    ? { text, sourceMode, compileProfile }
    : { text, sourceMode };
}

export function buildInitOptions(
  text,
  sourceMode = DEFAULT_SOURCE_MODE,
  compileProfile = DEFAULT_COMPILE_PROFILE,
  model,
) {
  const payload = buildSourceOptions(text, sourceMode, compileProfile);
  return model ? { ...payload, model } : payload;
}
