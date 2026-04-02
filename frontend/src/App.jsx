import React, { useEffect, useRef, useState } from 'react';
import { Scrollbar } from 'react-scrollbars-custom';
import CodeHeader from './components/CodeHeader.jsx';
import CodeEditor from './components/CodeEditor';
import Toolbar from './components/Toolbar';
import StepInfo from './components/StepInfo';
import TreeCanvas from './components/TreeCanvas';
import CustomAlert from './components/CustomAlert';
import useStepper from './hooks/useStepper';
import Resizable from './components/Resizable';
import Sidebar from './components/Sidebar';
import { exampleById } from './utils/example_programs.js';
import {
  DEFAULT_SEARCH_STRATEGY,
  HOIST_OPTIONS,
  SCHEDULER_OPTIONS,
} from './utils/search_strategy.js';
import {
  buildSourceOptions,
  CONJ_ASSOC_OPTIONS,
  DELAY_PLACEMENT_OPTIONS,
  DEFAULT_COMPILE_PROFILE,
  DEFAULT_SOURCE_MODE,
  DISJ_ASSOC_OPTIONS,
  SOURCE_MODE_OPTIONS,
} from './utils/source_defaults.js';
import {
  emptyResponseMessage,
  parseStepperPayload,
} from './utils/stepper_protocol.js';
import {
  deriveToolbarState,
} from './utils/app_state.js';
import {
  deriveEditableCodeState,
  deriveFrozenEditorState,
  deriveLoadedExampleState,
  deriveThawedEditorState,
} from './utils/editor_state.js';
import './styles.css';

function App() {
  const [code, setCode] = useState('');
  const originalCodeRef = useRef('');
  const initialTaggedCodeRef = useRef('');
  const [selectedExampleId, setSelectedExampleId] = useState('');
  const [selectedExampleSource, setSelectedExampleSource] = useState('');
  const [sourceMode, setSourceMode] = useState(DEFAULT_SOURCE_MODE);
  const [compileProfile, setCompileProfile] = useState(DEFAULT_COMPILE_PROFILE);
  const [searchStrategy, setSearchStrategy] = useState(DEFAULT_SEARCH_STRATEGY);
  const [isFrozen, setFrozen] = useState(false);
  const [isAtStart, setIsAtStart] = useState(true);
  const [isAtEnd, setIsAtEnd] = useState(false);
  const [alert, setAlert] = useState({ isOpen: false, message: '' });
  const treeRef = useRef();
  const [substitutionData, setSubstitutionData] = useState([]);
  const [trailData, setTrailData] = useState([]);
  const [goalId, setGoalId] = useState(null);
  const [stateId, setStateId] = useState(null);
  const [sidebarOpen, setSidebarOpen] = useState(false);
  const [isExampleLoading, setIsExampleLoading] = useState(false);
  const [darkMode, setDarkMode] = useState(false);

  const clearSelection = () => {
    setGoalId(null);
    setStateId(null);
    setSubstitutionData([]);
    setTrailData([]);
  };

  const {
    tree, stepInfo,
    init, step, reset, back, clear: clearStepper
  } = useStepper();

  const convertExampleToMicro = async (sourceText, profile = compileProfile) => {
    const response = await fetch('/api/post/source-convert', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        ...buildSourceOptions(sourceText, DEFAULT_SOURCE_MODE, profile),
        targetSourceMode: "micro",
      }),
      credentials: "include",
    });
    const payloadText = await response.text();
    if (payloadText.trim() === '') {
      throw new Error(emptyResponseMessage(response));
    }
    const payload = parseStepperPayload(payloadText);
    if (!response.ok) {
      throw new Error(payload?.error || `Unable to convert example (${response.status})`);
    }
    return payload.source;
  };

  const loadExampleSource = async (
    exampleId,
    nextSourceMode = sourceMode,
    nextCompileProfile = compileProfile,
  ) => {
    const example = exampleById(exampleId);
    if (!example) return null;
    return nextSourceMode === "mini"
      ? example.miniSource
      : convertExampleToMicro(example.miniSource, nextCompileProfile);
  };

  const applyExampleSource = (nextCode, exampleId) => {
    const nextState = deriveLoadedExampleState(exampleId, nextCode);
    setSelectedExampleId(nextState.selectedExampleId);
    setSelectedExampleSource(nextState.selectedExampleSource);
    setCode(nextState.code);
  };

  const handleInit = async () => {
    const trimmed = code.trim();
    if (!trimmed) {
      setAlert({ isOpen: true, message: "Program is empty." });
      return;
    }

    originalCodeRef.current = code;
    const [success, progOrError] = await init(code, sourceMode, compileProfile, searchStrategy);
    if (success) {
      clearSelection();
      const nextState = deriveFrozenEditorState(code, progOrError);
      originalCodeRef.current = nextState.originalCode;
      initialTaggedCodeRef.current = nextState.initialTaggedCode;
      setFrozen(nextState.isFrozen);
      setCode(nextState.code);
      setIsAtStart(nextState.isAtStart);
      setIsAtEnd(nextState.isAtEnd);
    } else {
      setAlert({ isOpen: true, message: progOrError });
    }
  };

  const handleStep = async () => {
    const [success, stepDone, error] = await step();
    if (!success) {
      setAlert({ isOpen: true, message: error });
      return;
    }
    setIsAtStart(false);
    setIsAtEnd(stepDone);
  };

  const handleBack = async () => {
    const [success, atStart, error] = await back();
    if (!success) {
      setAlert({ isOpen: true, message: error });
      return;
    }
    setIsAtStart(atStart);
    setIsAtEnd(false);
  };

  const handleReset = async () => {
    const [success, error] = await reset();
    if (!success) {
      setAlert({ isOpen: true, message: error });
      return;
    }
    clearSelection();
    const nextState = deriveThawedEditorState(originalCodeRef.current);
    setCode(nextState.code);
    setFrozen(nextState.isFrozen);
    setIsAtStart(nextState.isAtStart);
    setIsAtEnd(nextState.isAtEnd);
  };

  useEffect(() => {
    if (tree && treeRef.current) {
      treeRef.current.redraw(tree);
      treeRef.current.updateSidebar(stateId);
    }
  }, [tree, stateId]);

  useEffect(() => {
    if (!selectedExampleId) {
      setIsExampleLoading(false);
      return undefined;
    }
    let active = true;
    setIsExampleLoading(true);

    const load = async () => {
      try {
        const nextCode = await loadExampleSource(
          selectedExampleId,
          sourceMode,
          compileProfile,
        );
        if (!active || nextCode == null) return;
        applyExampleSource(nextCode, selectedExampleId);
      } catch (error) {
        if (!active) return;
        setAlert({
          isOpen: true,
          message: error?.message || "Unable to load example.",
        });
      } finally {
        if (active) {
          setIsExampleLoading(false);
        }
      }
    };

    load();
    return () => { active = false; };
  }, [selectedExampleId, sourceMode, compileProfile]);

  const toolbarState = deriveToolbarState({
    isFrozen,
    code,
    isExampleLoading,
    isAtStart,
    isAtEnd,
  });

  const handleSourceModeChange = (nextSourceMode) => {
    if (isFrozen) return;
    if (selectedExampleId) {
      setIsExampleLoading(true);
    }
    setSourceMode(nextSourceMode);
  };

  const handleCompileProfileChange = (axis, value) => {
    if (isFrozen) return;
    setCompileProfile((current) => ({ ...current, [axis]: value }));
  };

  const handleExampleChange = (exampleId) => {
    if (isFrozen) {
      clearSelection();
      clearStepper();
      const nextState = deriveThawedEditorState(originalCodeRef.current);
      setCode(nextState.code);
      setFrozen(nextState.isFrozen);
      setIsAtStart(nextState.isAtStart);
      setIsAtEnd(nextState.isAtEnd);
      initialTaggedCodeRef.current = '';
    }
    setIsExampleLoading(Boolean(exampleId));
    setSelectedExampleSource('');
    setSelectedExampleId(exampleId);
  };

  const handleSearchStrategyChange = (axis, value) => {
    if (isFrozen) return;
    setSearchStrategy((current) => ({ ...current, [axis]: value }));
  };

  const handleCodeChange = (nextCode) => {
    const nextState = deriveEditableCodeState(
      nextCode,
      selectedExampleId,
      selectedExampleSource,
    );
    setCode(nextState.code);
    setSelectedExampleId(nextState.selectedExampleId);
    setSelectedExampleSource(nextState.selectedExampleSource);
  };

  return (
    <div className="container">
      <Resizable>
        <div className="input-container">
          <CodeHeader
            logoSrc={darkMode ? "/mk_logo_white.png" : "/mk_logo_black.png"}
            exampleValue={selectedExampleId}
            onExampleChange={handleExampleChange}
            sourceModeValue={sourceMode}
            sourceModeOptions={SOURCE_MODE_OPTIONS}
            onSourceModeChange={handleSourceModeChange}
            compileProfile={compileProfile}
            conjAssocOptions={CONJ_ASSOC_OPTIONS}
            disjAssocOptions={DISJ_ASSOC_OPTIONS}
            delayPlacementOptions={DELAY_PLACEMENT_OPTIONS}
            onCompileProfileChange={handleCompileProfileChange}
            searchStrategy={searchStrategy}
            hoistOptions={HOIST_OPTIONS}
            schedulerOptions={SCHEDULER_OPTIONS}
            onSearchStrategyChange={handleSearchStrategyChange}
            isFrozen={isFrozen}
            isExampleLoading={isExampleLoading}
          />
          <div className="editor-area">
            <CodeEditor
              codeText={code}
              setCodeText={handleCodeChange}
              isFrozen={isFrozen}
              isDark={darkMode}
              goalId={goalId}
              onTagClick={setGoalId}
            />
          </div>
          <Toolbar
            onStart={handleInit}
            onStep={handleStep}
            onBack={handleBack}
            onReset={handleReset}
            canStart={toolbarState.canStart}
            canReset={toolbarState.canReset}
            canBack={toolbarState.canBack}
            canStep={toolbarState.canStep}
          />
        </div>

        <div className="right-pane">
          <StepInfo {...stepInfo} darkMode={darkMode} setDarkMode={setDarkMode} />
          <Scrollbar style={{ width: '100%', height: '100%' }}>
            <div style={{ display: 'block', width: 'max-content', margin: '0 auto' }}>
              <TreeCanvas
                ref={treeRef}
                onNodeClick={({ substitutionData, trailData, gId, sId }) => {
                  setSubstitutionData(substitutionData);
                  setTrailData(trailData);
                  setGoalId(gId);
                  setStateId(sId);
                }}
                selectedGoalId={goalId}
                selectedStateId={stateId}
              />
            </div>
          </Scrollbar>
        </div>
      </Resizable>
      <Sidebar
        substitutionData={substitutionData}
        trailData={trailData}
        isOpen={sidebarOpen}
        onToggle={() => setSidebarOpen((open) => !open)}
      />
      <CustomAlert
        isOpen={alert.isOpen}
        message={alert.message}
        onClose={() => setAlert({ isOpen: false, message: '' })}
      />
    </div>
  );
}

export default App;
