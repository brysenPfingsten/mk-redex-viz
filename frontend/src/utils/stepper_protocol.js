export function readStepperHeaders(response) {
  return {
    isAtStart: response?.headers?.get("X-Is-Start") === "true",
    isDone: response?.headers?.get("X-Done") === "true",
  };
}

export function emptyResponseMessage(response) {
  const status = response?.status ?? "unknown";
  const statusText = response?.statusText ? ` ${response.statusText}` : "";
  const ok = response?.ok ?? (typeof response?.status === "number"
    && response.status >= 200
    && response.status < 300);
  if (ok) {
    return "Server returned an empty response. The backend may be unavailable or the proxy lost its connection.";
  }
  return `Request failed (${status}${statusText}); server returned an empty response.`;
}

export function parseStepperPayload(text) {
  if (typeof text === "string" && text.trim() === "") {
    throw new Error(
      "Server returned an empty response. The backend may be unavailable or the proxy lost its connection.",
    );
  }
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new Error(`Failed to parse server response: ${error.message}`);
  }
}

export function readStepperView(payload) {
  return {
    tree: JSON.parse(payload.program),
    stepInfo: {
      step: payload.step,
      stepName: payload.stepName,
      stepKind: payload.stepKind,
      executionStatus: payload.executionStatus,
      answerCount: payload.answerCount,
      reductionCount: payload.reductionCount,
      advanceCount: payload.advanceCount,
      configuration: payload.configuration,
    },
  };
}

export function nextStepperAction(executionStatus) {
  return executionStatus === "paused"
    ? {
      kind: "public-operation",
      label: "Advance past Delay",
      description: "Request public advancement of the exposed Delay. Subsequent reduction steps evaluate and commit its result.",
    }
    : {
      kind: "reduction",
      label: "Reduction step",
      description: "Apply one reduction of the current configuration.",
    };
}

export function responseErrorMessage(response, payload) {
  if (payload && typeof payload.error === "string" && payload.error.trim() !== "") {
    return payload.error;
  }
  const status = response?.status ?? "unknown";
  const statusText = response?.statusText ? ` ${response.statusText}` : "";
  return `Request failed (${status}${statusText})`;
}

export function thrownErrorMessage(error) {
  if (error instanceof Error && error.message.trim() !== "") {
    return error.message;
  }
  return "Unexpected request failure.";
}
