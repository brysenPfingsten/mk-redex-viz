import React from "react";
import "../styles.css";

// const themeOptions = [
//   { value: "vs-dark", label: "Dark"  },
//   { value: "hc-black", label: "Black" },
//   { value: "hc-light", label: "Light" }
// ];

const modelOptions = [
  { value: "microKanren", label: "µKanren" },
  { value: "dmitry",      label: "Dmitry et al." }
];

export default function CodeHeader({
  logoSrc,
  // theme,
  // onThemeChange,
  model,
  onModelChange,
}) {
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
        onChange={onModelChange}
        value={model}
      >
        {renderOptions(modelOptions)}
      </select>
    </div>
  );
}
