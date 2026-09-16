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
import { SEMANTIC_STYLES } from './utils/drawing.js';
import { exampleById } from './utils/example_programs.js';
import {
  buildSearchStrategy,
  DEFAULT_SEARCH_MODEL,
  DEFAULT_SEARCH_STRATEGY,
  SCHEDULER_OPTIONS,
  SEARCH_MODEL_OPTIONS,
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
  deriveStateSelectionUpdate,
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
  const [searchModel, setSearchModel] = useState(DEFAULT_SEARCH_MODEL);
  const [latticeScheduler, setLatticeScheduler] = useState(DEFAULT_SEARCH_STRATEGY.scheduler);
  const [isFrozen, setFrozen] = useState(false);
  const [isInitializing, setInitializing] = useState(false);
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
    if (isFrozen || isInitializing || isExampleLoading) return;
    const trimmed = code.trim();
    if (!trimmed) {
      setAlert({ isOpen: true, message: "Program is empty." });
      return;
    }

    setInitializing(true);
    try {
      originalCodeRef.current = code;
      const [success, progOrError] = await init(
        code,
        sourceMode,
        compileProfile,
        buildSearchStrategy(searchModel, latticeScheduler),
      );
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
    } finally {
      setInitializing(false);
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
    if (isInitializing) return;
    if (isFrozen) return;
    if (selectedExampleId) {
      setIsExampleLoading(true);
    }
    setSourceMode(nextSourceMode);
  };

  const handleCompileProfileChange = (axis, value) => {
    if (isInitializing) return;
    if (isFrozen) return;
    setCompileProfile((current) => ({ ...current, [axis]: value }));
  };

  const handleExampleChange = (exampleId) => {
    if (isInitializing) return;
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

  const handleSchedulerChange = (scheduler) => {
    if (isInitializing) return;
    if (isFrozen) return;
    setLatticeScheduler(scheduler);
  };

  const handleSearchModelChange = (model) => {
    if (isInitializing) return;
    if (isFrozen) return;
    setSearchModel(model);
  };

  const handleCodeChange = (nextCode) => {
    if (isInitializing) return;
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
            searchModelValue={searchModel}
            searchModelOptions={SEARCH_MODEL_OPTIONS}
            onSearchModelChange={handleSearchModelChange}
            schedulerValue={latticeScheduler}
            schedulerOptions={SCHEDULER_OPTIONS}
            onSchedulerChange={handleSchedulerChange}
            isFrozen={isFrozen || isInitializing}
            isExampleLoading={isExampleLoading || isInitializing}
          />
          <div className="editor-area">
            <CodeEditor
              codeText={code}
              setCodeText={handleCodeChange}
              isFrozen={isFrozen || isInitializing}
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
            canStart={!isInitializing && toolbarState.canStart}
            canReset={!isInitializing && toolbarState.canReset}
            canBack={!isInitializing && toolbarState.canBack}
            canStep={!isInitializing && toolbarState.canStep}
            executionStatus={stepInfo.executionStatus}
          />
        </div>

        <div className="right-pane">
          <StepInfo {...stepInfo} darkMode={darkMode} setDarkMode={setDarkMode} />
          <div className="picture-key" aria-label="Picture legend">
            <div className="picture-key-kinds">
              {Object.entries(SEMANTIC_STYLES).map(([kind, style]) => (
                <span key={kind} className={`picture-key-${kind}`}
                  style={{ background: style.fill, borderColor: style.stroke, color: style.stroke }}>
                  {style.label}
                </span>
              ))}
            </div>
            <div>Each <code>intro […]</code> group belongs to its node; its source is shown below it.
              {' '}Dotted edges lead into suspended bodies.</div>
          </div>
          <Scrollbar style={{ width: '100%', height: '100%' }}>
            <div style={{ display: 'block', width: 'max-content', margin: '0 auto' }}>
              <TreeCanvas
                ref={treeRef}
                onNodeClick={(payload) => {
                  const stateUpdate = deriveStateSelectionUpdate(payload);
                  const { gId } = payload;

                  setGoalId(gId);
                  if (stateUpdate) {
                    setSubstitutionData(stateUpdate.substitutionData);
                    setTrailData(stateUpdate.trailData);
                    setStateId(stateUpdate.stateId);
                  }
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
        hasStateSelection={stateId !== null}
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
