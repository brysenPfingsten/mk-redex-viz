import React, { useEffect } from 'react';
import './toggleSwitch.css';

export default function ToggleSwitch({ checked, onChange }) {
  useEffect(() => {
    document.documentElement.classList.toggle('dark', checked);
  }, [checked]);

  return (
    <label className="toggle-switch">
      <input
        type="checkbox"
        checked={checked}
        onChange={e => onChange(e.target.checked)}
      />
      <span className="slider" />
    </label>
  );
}
