import React from 'react';
import ToggleSwitch from './toggleSwitch.jsx';

const statusText = {
  idle: 'Ready', running: 'Running', paused: 'Paused at Delay',
  complete: 'Completed', stuck: 'Stuck',
};

const actionText = {
  initialization: 'Initialization',
  reduction: 'Reduction',
  'public-operation': 'Public advancement',
};

export default function StepInfo({ step, stepName, stepKind, executionStatus, answerCount,
  reductionCount, advanceCount, configuration, darkMode, setDarkMode }) {
  return (
    <div id="step-info" className="step-info-container">
      <div className="step-info-header">
        <div>
          History position: {step}
          {Number.isInteger(reductionCount) && Number.isInteger(advanceCount) && (
            <> · {reductionCount} reductions · {advanceCount} public advances</>
          )}<br/>
          {stepName && <>Last action: <span className={`step-action step-action-${stepKind}`}>
            {actionText[stepKind] ?? 'Operation'}
          </span>: {stepName}<br/></>}
          {statusText[executionStatus] ?? 'Ready'}
          {Number.isInteger(answerCount) && <> · {answerCount} committed answers</>}
        </div>
        <div style={{ marginRight: '50px' }}>
        <ToggleSwitch checked={darkMode} onChange={setDarkMode} />
      </div>
      </div>
      {typeof configuration === 'string' && (
        <details className="configuration-panel">
          <summary>Underlying configuration</summary>
          <p>The current backend configuration, including source identities and allocation owners.</p>
          <pre tabIndex={0} aria-label="Underlying configuration">{configuration}</pre>
        </details>
      )}
    </div>
  );
}
