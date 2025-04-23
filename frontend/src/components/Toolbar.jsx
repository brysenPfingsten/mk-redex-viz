import React from 'react';

export default function Toolbar({ onDebug, onStep, onBack, onReset, disabled }) {
    return (
      <div className="button-container">
        <button onClick={onDebug} disabled={disabled}>Debug</button>
        <button onClick={onReset} disabled={disabled}>Reset</button>
        <button onClick={onBack} disabled={disabled}>Back</button>
        <button onClick={onStep} disabled={disabled}>Step</button>
      </div>
    );
  }
  