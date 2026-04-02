import React from "react";
import "../styles.css";
import { examplesForModel } from "../utils/example_programs.js";

export default function CodeHeader({
  logoSrc,
  programText,
  onProgramChange,
  modelValue,
  modelOptions = [],
  onModelChangeRequest,
  isFrozen,
  analysisStatus = "idle",
  compatWarning = null,
  onSwitchCompatibleModel = () => {},
}) {
  const availableExamples = examplesForModel(modelValue);

  const renderExampleOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  const renderModelOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  const changeModel = async (newModel) => {
    try {
      await onModelChangeRequest(newModel);
    } catch (_) {
      // Keep current model selection when request fails.
    }
  };

  return (
    <div className="code-header">
      <a href="https://minikanren.org" target="_blank">
        <img src={logoSrc} alt="Logo" className="logo"/>
      </a>

      <select
        className="select"
        value={programText}
        onChange={(e) => onProgramChange(e.target.value)}
        disabled={isFrozen}
      >
        {renderExampleOptions(availableExamples)}
      </select>

      <select
        className="select"
        value={modelValue}
        onChange={(e) => changeModel(e.target.value)}
        disabled={isFrozen}
      >
        {renderModelOptions(modelOptions)}
      </select>

      {analysisStatus === "analyzing" && !isFrozen ? (
        <div style={{ marginLeft: "10px", fontSize: "0.85rem" }}>
          Analyzing...
        </div>
      ) : null}

      {compatWarning ? (
        <div
          style={{
            marginLeft: "10px",
            padding: "8px 10px",
            border: "1px solid #b55",
            borderRadius: "6px",
            background: "#fff5f5",
            color: "#622",
            maxWidth: "560px",
          }}
        >
          <div style={{ marginBottom: "6px" }}>{compatWarning.message}</div>
          {compatWarning.reasons && compatWarning.reasons.length > 0 ? (
            <div style={{ marginBottom: "6px", fontSize: "0.85rem" }}>
              {compatWarning.reasons.join("; ")}
            </div>
          ) : null}
          <div style={{ display: "flex", gap: "8px" }}>
            <button
              type="button"
              onClick={onSwitchCompatibleModel}
              disabled={isFrozen || !compatWarning.canSwitchModel}
            >
              Switch to Compatible Model
            </button>
          </div>
        </div>
      ) : null}
    </div>
  );
}
