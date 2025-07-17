import React, { useState } from "react";
import "../styles.css";

// const themeOptions = [
//   { value: "vs-dark", label: "Dark"  },
//   { value: "hc-black", label: "Black" },
//   { value: "hc-light", label: "Light" }
// ];

const modelOptions = [
  { value: "microKanren", label: "µKanren" },
  { value: "no-rr",       label: "No Railway"},
  { value: "dmitry",      label: "Dmitry et al." },
  { value: "dfs",         label: "DFS"},
];

export default function CodeHeader({
  logoSrc,
  // theme,
  // onThemeChange,
}) {
  const [model, setModel] = useState('');

  const renderOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  // TODO: Maybe add some error handling here
  const changeModel = async (newModel) => {
    setModel(newModel);
    fetch('api/post/model', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json'},
      body: JSON.stringify({ model: newModel})
    });
  }

  return (
    <div className="code-header">
      <a href="https://minikanren.org" target="_blank">
        <img src={logoSrc} alt="Logo" className="logo"/>
      </a>

      {/*  TODO: Add theme options here for editor */}
      {/* <select */}
      {/*   className="select" */}
      {/*   onChange={onThemeChange} */}
      {/*   value={theme} */}
      {/* > */}
      {/*   {renderOptions(themeOptions)} */}
      {/* </select> */}

      <select
        className="select"
        onChange={(e) => changeModel(e.target.value)}
        value={model}
      >
        {renderOptions(modelOptions)}
      </select>
    </div>
  ); }
