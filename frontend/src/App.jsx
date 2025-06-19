import React, { useState, useRef, useEffect } from 'react';
import { Scrollbar } from 'react-scrollbars-custom';
import CodeHeader      from './components/CodeHeader.jsx';
import CodeEditor      from './components/CodeEditor';
import Toolbar         from './components/Toolbar';
import StepInfo        from './components/StepInfo';
import TreeCanvas      from './components/TreeCanvas';
import CustomAlert     from './components/CustomAlert';
import useStepper      from './hooks/useStepper';
import Resizable       from './components/Resizable';
import Sidebar from './components/Sidebar';
import './styles.css'

function App() {
  const [code, setCode] = useState('');
  const originalCodeRef = useRef('');
  const [theme, setTheme] = useState('');
  const [model, setModel] = useState('');
  const [isFrozen, setFrozen] = useState(false);
  const [alert, setAlert] = useState({ isOpen: false, message: '' });
  const {
    tree, stepInfo,
    init, step, reset, back
  } = useStepper({
    onSuccess: () => {
      setSelectedId(null);
      setDisabled(prev => ({ ...prev, back: false, reset: false }));
    }
  });
  const [disabled, setDisabled] = useState({
    debug: false,
    reset: true,
    back: true,
    step: true,
  });
  const [substitutionData, setSubstitutionData] = useState([]);
  const [trailData, setTrailData] = useState([]);
  const [selectedId, setSelectedId] = useState(null);
  const [sidebarOpen, setSidebarOpen] = useState(false);
  
  const [ darkMode, setDarkMode ] = useState(false);
  const svgRef = useRef();
  const scrollRef = useRef(null);

  const handleInit = async () => {
    originalCodeRef.current = code;
    const [success, progOrError] = await init(code);
    if (success) {
      setFrozen(true);
      setCode(progOrError);
      setDisabled({debug: true, reset: false, back: true, step: false});
    } else {
      setAlert({ isOpen: true, message: progOrError });
    }
  };
  
  const handleStep = async () => {
    const [success, isDone] = await step();
    if (isDone) { // no more reductions
      setDisabled({debug: true, reset: false, back: false, step: true});
    } else if (success) { // success but more reductions
      setDisabled(prev => ({...prev, back: false}));
    }
  };

  const handleBack = async () => {
    const [_, isLast] = await back();
    if (isLast) {  // TODO: Change this to isStart on both sides
      setDisabled(prev => ({...prev, back: true}));
    } else {
      setDisabled(prev => ({...prev, step: false}));
    }
  }

  const handleReset = async () => {
    const success = await reset();  
    if (success) {
      setCode(originalCodeRef.current);
      setFrozen(false);
      setDisabled({debug: false, reset: true, back: true, step: true});
    }
  }
  
  useEffect(() => {
    if (tree && svgRef.current) {
      svgRef.current.redraw(tree);
      }
  }, [tree]);

  return (
    <div className="container">
      <Resizable>
        <div className="input-container">
          <CodeHeader
            logoSrc={darkMode ? "/mk_logo_white.png" : "/mk_logo_black.png"}
            model={model}
            onModelChange={(e) => setModel(e.target.value)}
           />
          <div className="editor-area">
            <CodeEditor 
              codeText={code} 
              setCodeText={setCode} 
              isFrozen={isFrozen} 
              isDark={darkMode}
              selectedId={selectedId}
              onTagClick={setSelectedId}
            />
          </div>
          <Toolbar
            onDebug={handleInit}
            onStep={handleStep}
            onBack={handleBack}
            onReset={handleReset}
            disabled={disabled}
          />
        </div>
        
        <div className="right-pane">
          <StepInfo {...stepInfo} darkMode={darkMode} setDarkMode={setDarkMode} />
          <Scrollbar style={{ width: '100%', height: '100%' }}>
            <div style={{ display: 'block', width: 'max-content', margin: '0 auto' }}>
              <TreeCanvas 
                ref={svgRef}
                onNodeClick={({ substitutionData, trailData, id }) => {
                  setSubstitutionData(substitutionData);
                  setTrailData(trailData);
                  setSelectedId(id);
                }}
                selectedId={selectedId}
                />
            </div>
          </Scrollbar>
        </div>
      </Resizable>  
      <Sidebar
        substitutionData={substitutionData} 
        trailData={trailData} 
        isOpen={sidebarOpen}
        onToggle={() => setSidebarOpen(o => !o)}
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
