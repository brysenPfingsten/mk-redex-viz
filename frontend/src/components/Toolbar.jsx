import React from 'react';

export default function Toolbar({ onStart, onStep, onBack, onReset, disabled }) {
    return (
      <div className="button-container">
        <button onClick={onStart} disabled={disabled.start}>Start</button>
        <button onClick={onReset} disabled={disabled.reset}>Reset</button>
        <button onClick={onBack} disabled={disabled.back}>Back</button>
        <button onClick={onStep} disabled={disabled.step}>Step</button>
      </div>
    );
  }
  
