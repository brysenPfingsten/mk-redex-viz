import React, { forwardRef, useRef, useEffect, useMemo } from 'react';
import Editor from '@monaco-editor/react';
import { conf, language } from '../utils/minikanren-language';
import { parseTaggedText } from '../utils/tagged_source.js';
import { selectedSourceSegments } from '../utils/source_mapping.js';
import '../styles.css';

const CodeEditor = forwardRef(({ 
  codeText, setCodeText, 
  isFrozen, isDark,
  goalId, onTagClick,
}, ref) => {
  const editorRef = useRef(null);
  const monacoRef = useRef(null);
  const decorationIds = useRef([]);
  const segmentsRef = useRef([]);
  const isMounted = useRef(false);
  const pendingProgrammaticValue = useRef(null);

  // Memoize parsed results
  const { plain, segments } = useMemo(() => 
    isFrozen ? parseTaggedText(codeText) : { plain: codeText, segments: [] },
    [isFrozen, codeText]
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
    pendingProgrammaticValue.current = plain;
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

    const selDecs = selectedSourceSegments(segmentsRef.current, goalId)
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
      pendingProgrammaticValue.current = plain;
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
  }, [isFrozen, plain]);

  useEffect(() => {
      updateDecorations()
    },
    [goalId, plain, isFrozen]);

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
        onChange={(value) => {
          const nextValue = value ?? '';
          if (pendingProgrammaticValue.current === nextValue) {
            pendingProgrammaticValue.current = null;
            return;
          }
          if (!isFrozen) {
            setCodeText(nextValue);
          }
        }}
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
