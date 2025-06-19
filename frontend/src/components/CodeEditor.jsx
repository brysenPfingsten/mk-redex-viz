import React, { forwardRef, useRef, useEffect, useMemo } from 'react';
import Editor from '@monaco-editor/react';
import { conf, language } from '../utils/minikanren-language';
import '../styles.css';

function parseTaggedText(raw) {
  const segments = [];
  let plain = "";
  let lastIndex = 0;
  const stack = [];

  const markerRE = /\[\[\/?([A-Za-z0-9]+)\]\]/g;
  let m;
  while ((m = markerRE.exec(raw))) {
    const full  = m[0];
    const id    = m[1];
    const close = full.startsWith("[[/");
    const idx   = m.index;

    // 1) copy everything up to this marker into "plain" code
    plain += raw.slice(lastIndex, idx);

    if (!close) {
      // 2) opening tag: push its start position
      stack.push({ id, start: plain.length });
    } else {
      // 3) closing tag: pop the matching opener
      for (let i = stack.length - 1; i >= 0; --i) {
        if (stack[i].id === id) {
          const { start } = stack[i];
          stack.splice(i, 1);
          // record one segment from opener to current plain‐text length
          segments.push({ id, start, end: plain.length });
          break;
        }
      }
    }
    lastIndex = idx + full.length;
  }
  // 4) append whatever’s left after the last marker
  plain += raw.slice(lastIndex);
  return { plain, segments };
}

const CodeEditor = forwardRef(({ 
  codeText, setCodeText, 
  isFrozen, isDark,
  selectedId, onTagClick 
}, ref) => {
  const editorRef = useRef(null);
  const monacoRef = useRef(null);
  const decorationIds = useRef([]);
  const segmentsRef = useRef([]);
  const isMounted = useRef(false);

  // Memoize parsed results
  const { plain, segments } = useMemo(() => 
    isFrozen ? parseTaggedText(codeText) : { plain: codeText, segments: [] },
    [isFrozen]
  );

  // Keep segments reference updated
  useEffect(() => {
    segmentsRef.current = segments;
  }, [segments]);

  const handleEditorWillMount = (monaco) => {
    monaco.languages.register({ id: 'minikanren' });
    monaco.languages.setLanguageConfiguration('minikanren', conf);
    monaco.languages.setMonarchTokensProvider('minikanren', language);
  };

  const handleEditorDidMount = (editor, monaco) => {
    editorRef.current = editor;
    monacoRef.current = monaco;
    isMounted.current = true;

    // Set initial value
    editor.setValue(plain);

    editor.onMouseUp((e) => {
      const pos = e.target.position;
      if (!pos) return;
      
      const model = editor.getModel();
      const offset = model.getOffsetAt(pos);

      for (const seg of segmentsRef.current) {
        if (offset >= seg.start && offset < seg.end) {
          onTagClick(seg.id);
          break;
        }
      }
    });
  };

  const updateDecorations = () => {
    const editor = editorRef.current;
    const monaco = monacoRef.current;
    if (!editor || !monaco) return;

    const model = editor.getModel();
    if (!model) return;

    const baseDecs = segmentsRef.current.map(seg => {
      const start = model.getPositionAt(seg.start);
      const end = model.getPositionAt(seg.end);
      return {
        range: new monaco.Range(start.lineNumber, start.column, end.lineNumber, end.column),
        options: { inlineClassName: 'hidden-tag-deco' }
      };
    });

    const selDecs = selectedId == null ? [] : segmentsRef.current
      .filter(s => s.id === selectedId)
      .map(seg => {
        const start = model.getPositionAt(seg.start);
        const end = model.getPositionAt(seg.end);
        return {
          range: new monaco.Range(start.lineNumber, start.column, end.lineNumber, end.column),
          options: { inlineClassName: 'selected-tag-deco' }
        };
      });

    decorationIds.current = editor.deltaDecorations(
      decorationIds.current,
      [...baseDecs, ...selDecs]
    );
  };

  useEffect(() => {
    if (!isMounted.current) return;
    
    const editor = editorRef.current;
    if (!editor) return;

    // Update editor content when unfrozen
    if (!isFrozen && editor.getValue() !== plain) {
      editor.setValue(plain);
    }

    // Clear decorations when unfrozen
    if (!isFrozen) {
      decorationIds.current = editor.deltaDecorations(
        decorationIds.current,
        []
      );
      return;
    }

    updateDecorations();
  }, [isFrozen, selectedId, plain]);

  useEffect(() => {
    return () => {
      isMounted.current = false;
    };
  }, []);

  return (
    <div className="input-container">
      <Editor
        className="monaco-editor"
        height="100%"
        language="minikanren"
        value={plain}
        beforeMount={handleEditorWillMount}
        onMount={handleEditorDidMount}
        onChange={(value) => !isFrozen && setCodeText(value ?? '')}
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
    </div>
  );
});

export default CodeEditor;
