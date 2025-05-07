import React from 'react';

export default function Toolbar({ onDebug, onStep, onBack, onReset, disabled }) {
    return (
      <div className="button-container">
        <button onClick={onDebug} disabled={disabled.debug}>Debug</button>
        <button onClick={onReset} disabled={disabled.reset}>Reset</button>
        <button onClick={onBack} disabled={disabled.back}>Back</button>
        <button onClick={onStep} disabled={disabled.step}>Step</button>
      </div>
    );
  }
  