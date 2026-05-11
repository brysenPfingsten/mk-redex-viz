import React, { useState } from "react";
import "../styles.css";
import { exampleProgs } from "../utils/example_programs.js";

const modelOptions = [
  { value: "microKanren", label: "µKanren" },
  { value: "dfs", label: "DFS" },
  { value: "no-railway", label: "No Railway" },
  // { value: "dmitry",      label: "Dmitry et al." },
];

export default function CodeHeader({
  logoSrc,
  programText,
  onProgramChange,
  modelValue,
  onModelChange,
  isFrozen,
}) {
  const renderOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  // TODO: Maybe add some error handling here
  const changeModel = async (newModel) => {
    onModelChange(newModel);
    fetch("api/post/model", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model: newModel }),
    });
  };

  return (
    <div className="code-header">
      <a href="https://minikanren.org" target="_blank">
        <img src={logoSrc} alt="Logo" className="logo" />
      </a>

      <select
        className="select"
        value={programText}
        onChange={(e) => onProgramChange(e.target.value)}
        disabled={isFrozen}
      >
        {renderOptions(exampleProgs)}
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
  );
}
