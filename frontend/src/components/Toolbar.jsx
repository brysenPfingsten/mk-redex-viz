import React from 'react';
import { nextStepperAction } from '../utils/stepper_protocol.js';

export default function Toolbar({ onStart, onStep, onBack, onReset, canStart, canReset,
  canBack, canStep, executionStatus }) {
    const nextAction = nextStepperAction(executionStatus);
    return (
      <div className="trace-controls">
        <div className="control-section control-section-trace">
          <div className="control-section-title">Trace</div>
          <div className="button-container">
            <button onClick={onStart} disabled={!canStart}>Start</button>
            <button onClick={onReset} disabled={!canReset}>Reset</button>
            <button onClick={onBack} disabled={!canBack}>Back</button>
            <button onClick={onStep} disabled={!canStep}
              className={`next-action next-action-${nextAction.kind}`}
              title={nextAction.description}>{nextAction.label}</button>
          </div>
        </div>
      </div>
    );
  }
  
