import React, { useState, forwardRef } from 'react';
import '../styles.css'

const CodeEditor = forwardRef(({ onInit }, ref) => {
  const [code, setCode] = useState('');
  return (
    <div className="editor-wrapper">
      <textarea
        id="code-input"
        value={code}
        onChange={e => setCode(e.target.value)}
        placeholder="Enter your miniKanren query..."
      />
      <div id="highlight-overlay" className="overlay"></div>
    </div>
  );
});

export default CodeEditor;