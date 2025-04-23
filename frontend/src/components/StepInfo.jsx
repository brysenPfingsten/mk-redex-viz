import React from 'react';
import ToggleSwitch from './toggleSwitch.jsx';

export default function StepInfo({ step, stepName, darkMode, setDarkMode }) {
  return (
    <div id="step-info" className="step-info-container">
      <div className="step-info-header">
        <div>
          Step: {step}<br/>
          Reduction Step: {stepName}
        </div>
        <ToggleSwitch checked={darkMode} onChange={setDarkMode} />
      </div>
    </div>
  );
}
