import React, { forwardRef } from 'react';
import Editor from '@monaco-editor/react';
import { conf, language } from '../utils/minikanren-language';
import '../styles.css';

const CodeEditor = forwardRef(({ codeText, setCodeText, isFrozen, isDark }, ref) => {
  const handleEditorWillMount = (monaco) => {
    monaco.languages.register({ id: 'minikanren' });
    monaco.languages.setLanguageConfiguration('minikanren', conf);
    monaco.languages.setMonarchTokensProvider('minikanren', language);
  };

  return (
    <div className="input-container">
      <Editor
        className="monaco-editor"
        height="100%"
        language="minikanren"
        value={codeText}
        beforeMount={handleEditorWillMount}
        onChange={(value) => setCodeText(value ?? '')}
        theme={isDark ? 'vs-dark' : 'vs-light'}
        options={{
          readOnly: isFrozen,
          minimap: { enabled: false },
          matchBrackets: 'always',
          tabSize: 2,
          fontSize: 14,
          lineNumbers: 'on',
          wordWrap: 'off',
          scrollBeyondLastLine: false,
          automaticLayout: true
        }}
      />
      <div id="highlight-overlay" className="overlay"></div>
    </div>
  );
});

export default CodeEditor;