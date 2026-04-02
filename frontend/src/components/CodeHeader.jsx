import React from "react";
import "../styles.css";
import { exampleOptions } from "../utils/example_programs.js";

export default function CodeHeader({
  logoSrc,
  exampleValue,
  onExampleChange,
  sourceModeValue,
  sourceModeOptions = [],
  onSourceModeChange,
  compileProfile,
  conjAssocOptions = [],
  disjAssocOptions = [],
  delayPlacementOptions = [],
  onCompileProfileChange,
  modelValue,
  modelOptions = [],
  onModelChange,
  isFrozen,
  analysisStatus = "idle",
  compatWarning = null,
  onSwitchCompatibleModel = () => {},
}) {
  const availableExamples = exampleOptions();

  const renderOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  return (
    <div className="code-header">
      <a href="https://minikanren.org" target="_blank">
        <img src={logoSrc} alt="Logo" className="logo"/>
      </a>

      <div className="header-controls">
        <label className="select-group">
          <span className="select-label">Example</span>
          <select
            className="select"
            value={exampleValue}
            onChange={(e) => onExampleChange(e.target.value)}
            disabled={isFrozen}
          >
            {renderOptions(availableExamples)}
          </select>
        </label>

        <label className="select-group">
          <span className="select-label">Source</span>
          <select
            className="select"
            value={sourceModeValue}
            onChange={(e) => onSourceModeChange(e.target.value)}
            disabled={isFrozen}
          >
            {renderOptions(sourceModeOptions)}
          </select>
        </label>

        <label className="select-group">
          <span className="select-label">Conj</span>
          <select
            className="select"
            value={compileProfile.conjAssoc}
            onChange={(e) => onCompileProfileChange("conjAssoc", e.target.value)}
            disabled={isFrozen}
          >
            {renderOptions(conjAssocOptions)}
          </select>
        </label>

        <label className="select-group">
          <span className="select-label">Disj</span>
          <select
            className="select"
            value={compileProfile.disjAssoc}
            onChange={(e) => onCompileProfileChange("disjAssoc", e.target.value)}
            disabled={isFrozen}
          >
            {renderOptions(disjAssocOptions)}
          </select>
        </label>

        <label className="select-group">
          <span className="select-label">Delay</span>
          <select
            className="select"
            value={compileProfile.delayPlacement}
            onChange={(e) => onCompileProfileChange("delayPlacement", e.target.value)}
            disabled={isFrozen}
          >
            {renderOptions(delayPlacementOptions)}
          </select>
        </label>

        <label className="select-group">
          <span className="select-label">Model</span>
          <select
            className="select"
            value={modelValue}
            onChange={(e) => onModelChange(e.target.value)}
            disabled={isFrozen}
          >
            {renderOptions(modelOptions)}
          </select>
        </label>
      </div>

      {analysisStatus === "analyzing" && !isFrozen ? (
        <div style={{ fontSize: "0.85rem" }}>
          Analyzing...
        </div>
      ) : null}

      {compatWarning ? (
        <div
          style={{
            flexBasis: "100%",
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
