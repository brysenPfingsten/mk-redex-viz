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
  searchStrategy,
  hoistOptions = [],
  schedulerOptions = [],
  onSearchStrategyChange,
  isFrozen,
  isExampleLoading = false,
}) {
  const availableExamples = exampleOptions();

  const renderOptions = (opts) =>
    opts.map(({ value, label }) => (
      <option key={value} value={value}>
        {label}
      </option>
    ));

  const renderRadioGroup = (groupLabel, name, value, options, onChange) => (
    <fieldset className="radio-group">
      <legend className="select-label">{groupLabel}</legend>
      <div className="radio-options">
        {options.map(({ value: optionValue, label }) => (
          <label
            key={optionValue}
            className={[
              "radio-option",
              value === optionValue ? "checked" : "",
              isFrozen ? "disabled" : "",
            ].filter(Boolean).join(" ")}
          >
            <input
              type="radio"
              name={name}
              value={optionValue}
              checked={value === optionValue}
              onChange={(e) => onChange(e.target.value)}
              disabled={isFrozen}
            />
            <span>{label}</span>
          </label>
        ))}
      </div>
    </fieldset>
  );

  return (
    <div className="code-header">
      <a href="https://minikanren.org" target="_blank" rel="noreferrer">
        <img src={logoSrc} alt="Logo" className="logo"/>
      </a>

      <div className="header-controls">
        <section className="control-section control-section-program">
          <div className="control-section-title">Source</div>
          <div className="control-section-body">
            <label className="select-group">
              <span className="select-label">Example</span>
              <select
                className="select"
                value={exampleValue}
                onChange={(e) => onExampleChange(e.target.value)}
                disabled={isExampleLoading}
              >
                {renderOptions(availableExamples)}
              </select>
            </label>

            {renderRadioGroup(
              "Source Mode",
              "source-mode",
              sourceModeValue,
              sourceModeOptions,
              onSourceModeChange,
            )}
          </div>
        </section>

        {sourceModeValue === "mini" && (
          <section className="control-section control-section-compile">
            <div className="control-section-title">Lowering</div>
            <div className="control-section-body">
              {renderRadioGroup(
                "Conj Associativity",
                "conj-assoc",
                compileProfile.conjAssoc,
                conjAssocOptions,
                (value) => onCompileProfileChange("conjAssoc", value),
              )}

              {renderRadioGroup(
                "Disj Associativity",
                "disj-assoc",
                compileProfile.disjAssoc,
                disjAssocOptions,
                (value) => onCompileProfileChange("disjAssoc", value),
              )}

              {renderRadioGroup(
                "Delay Placement",
                "delay-placement",
                compileProfile.delayPlacement,
                delayPlacementOptions,
                (value) => onCompileProfileChange("delayPlacement", value),
              )}
            </div>
          </section>
        )}

        <section className="control-section control-section-search">
          <div className="control-section-title">Search</div>
          <div className="control-section-body">
            {renderRadioGroup(
              "Hoist",
              "search-hoist",
              searchStrategy.hoist,
              hoistOptions,
              (value) => onSearchStrategyChange("hoist", value),
            )}

            {renderRadioGroup(
              "Scheduler",
              "search-scheduler",
              searchStrategy.scheduler,
              schedulerOptions,
              (value) => onSearchStrategyChange("scheduler", value),
            )}
          </div>
        </section>
      </div>
    </div>
  );
}
