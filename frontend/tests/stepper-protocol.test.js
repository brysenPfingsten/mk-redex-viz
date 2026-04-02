import test from "node:test";
import assert from "node:assert/strict";

import {
  parseStepperPayload,
  readStepperHeaders,
  responseErrorMessage,
  thrownErrorMessage,
} from "../src/utils/stepper_protocol.js";

function makeResponse({ status = 200, statusText = "OK", isAtStart = false, isDone = false } = {}) {
  return {
    status,
    statusText,
    headers: {
      get(name) {
        if (name === "X-Is-Start") return isAtStart ? "true" : "false";
        if (name === "X-Done") return isDone ? "true" : "false";
        return null;
      },
    },
  };
}

test("readStepperHeaders reads semantic navigation flags", () => {
  assert.deepEqual(
    readStepperHeaders(makeResponse({ isAtStart: true, isDone: true })),
    { isAtStart: true, isDone: true },
  );
});

test("parseStepperPayload parses JSON null and structured payloads", () => {
  assert.equal(parseStepperPayload("null"), null);
  assert.deepEqual(parseStepperPayload('{"step":1,"stepName":"foo"}'), {
    step: 1,
    stepName: "foo",
  });
});

test("parseStepperPayload reports malformed JSON clearly", () => {
  assert.throws(
    () => parseStepperPayload("<html>boom</html>"),
    /Failed to parse server response/,
  );
});

test("responseErrorMessage prefers backend error text and falls back to HTTP status", () => {
  assert.equal(
    responseErrorMessage(makeResponse({ status: 400, statusText: "Bad Request" }), { error: "Bad input" }),
    "Bad input",
  );
  assert.equal(
    responseErrorMessage(makeResponse({ status: 500, statusText: "Internal Server Error" }), null),
    "Request failed (500 Internal Server Error)",
  );
});

test("thrownErrorMessage always returns a user-facing message", () => {
  assert.equal(thrownErrorMessage(new Error("Network down")), "Network down");
  assert.equal(thrownErrorMessage("boom"), "Unexpected request failure.");
});
