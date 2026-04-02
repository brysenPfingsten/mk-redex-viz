import React, { useEffect } from "react";
import "../styles.css";
import { examplesForModel } from "../utils/example_programs.js";

export default function CodeHeader({
  logoSrc,
  programText,
  onProgramChange,
  modelValue,
  modelOptions = [],
  onModelChange,
  isFrozen,
}) {
  const availableExamples = examplesForModel(modelValue);

  useEffect(() => {
    const stillAvailable = availableExamples.some((opt) => opt.value === programText);
    if (!stillAvailable) onProgramChange("");
  }, [availableExamples, programText, onProgramChange]);

  const renderOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  // TODO: Maybe add some error handling here
  const changeModel = async (newModel) => {
    try {
      const response = await fetch('api/post/model', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json'},
        body: JSON.stringify({ model: newModel})
      });
      if (response.ok) {
        onModelChange(newModel);
      }
    } catch (_) {
      // Keep current model selection when request fails.
    }
  }

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
        {renderOptions(availableExamples)}
      </select>

      <select
        className="select"
        value={modelValue}
        onChange={(e) => changeModel(e.target.value)}
        disabled={isFrozen}
      >
        {renderOptions(modelOptions)}
      </select>
    </div>
  ); }
