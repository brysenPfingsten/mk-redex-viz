export function readStepperHeaders(response) {
  return {
    isAtStart: response?.headers?.get("X-Is-Start") === "true",
    isDone: response?.headers?.get("X-Done") === "true",
  };
}

export function parseStepperPayload(text) {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new Error(`Failed to parse server response: ${error.message}`);
  }
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
