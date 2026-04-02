import test from "node:test";
import assert from "node:assert/strict";

import { termToString } from "../src/utils/strings.js";

test("termToString renders dotted-pair JSON explicitly", () => {
  assert.equal(
    termToString({ pair: ["_.0", "_.1"] }),
    "(_.0 . _.1)",
  );
});
