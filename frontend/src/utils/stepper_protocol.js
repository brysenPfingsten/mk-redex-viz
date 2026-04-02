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
