import test from "node:test";
import assert from "node:assert/strict";
import { analysisStatusForModel, isStartBlockedByAnalysis } from "../src/utils/compatibility.js";

test("analysisStatusForModel returns syntax-error for invalid syntax", () => {
  assert.equal(analysisStatusForModel({ validSyntax: false }, "mk-l4-rail-lazy"), "syntax-error");
});

test("analysisStatusForModel returns ok when current model is compatible", () => {
  const analysis = {
    validSyntax: true,
    compatibleModelIds: ["mk-l4-rail-lazy", "mk-l3-flip-lazy"],
  };
  assert.equal(analysisStatusForModel(analysis, "mk-l4-rail-lazy"), "ok");
});

test("analysisStatusForModel returns incompatible when current model is not compatible", () => {
  const analysis = {
    validSyntax: true,
    compatibleModelIds: ["mk-l4-rail-lazy"],
  };
  assert.equal(analysisStatusForModel(analysis, "mk-l0-core"), "incompatible");
});

test("isStartBlockedByAnalysis blocks on empty program", () => {
  assert.equal(isStartBlockedByAnalysis({
    isFrozen: false,
    code: "   ",
    analysisStatus: "idle",
  }), true);
});

test("isStartBlockedByAnalysis blocks on syntax-error/incompatible", () => {
  for (const status of ["syntax-error", "incompatible"]) {
    assert.equal(isStartBlockedByAnalysis({
      isFrozen: false,
      code: "(run* (q) (== q 'ok))",
      analysisStatus: status,
    }), true);
  }
});

test("isStartBlockedByAnalysis does not block on analyzing", () => {
  assert.equal(isStartBlockedByAnalysis({
    isFrozen: false,
    code: "(run* (q) (== q 'ok))",
    analysisStatus: "analyzing",
  }), false);
});

test("isStartBlockedByAnalysis allows start on ok + non-empty + unfrozen", () => {
  assert.equal(isStartBlockedByAnalysis({
    isFrozen: false,
    code: "(run* (q) (== q 'ok))",
    analysisStatus: "ok",
  }), false);
});

test("isStartBlockedByAnalysis does not force block in frozen mode", () => {
  assert.equal(isStartBlockedByAnalysis({
    isFrozen: true,
    code: "(run* (q) (== q 'ok))",
    analysisStatus: "incompatible",
  }), false);
});
