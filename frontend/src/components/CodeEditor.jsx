import React, { forwardRef } from 'react';
import '../styles.css';

const CodeEditor = forwardRef(({ codeText, setCodeText }, ref) => {
  return (
    <div className="editor-wrapper">
      <textarea
        id="code-input"
        value={codeText}
        onChange={e => setCodeText(e.target.value)}
        placeholder="Enter your miniKanren query..."
      />
      <div id="highlight-overlay" className="overlay"></div>
    </div>
  );
});

export default CodeEditor;