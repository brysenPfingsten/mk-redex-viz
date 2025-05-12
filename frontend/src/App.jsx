import { useState, useRef, useEffect } from 'react';
import React from 'react';
import { Scrollbar } from 'react-scrollbars-custom';
import CodeEditor      from './components/CodeEditor';
import Toolbar         from './components/Toolbar';
import StepInfo        from './components/StepInfo';
import TreeCanvas      from './components/TreeCanvas';
import useStepper      from './hooks/useStepper';
import Resizable       from './components/Resizable';
import './styles.css'

function App() {
  const [codeText, setCodeText] = useState('');
  const [isFrozen, setFrozen] = useState(false);
  const {
    tree, stepInfo,
    init, step, reset, back
  } = useStepper({
    onInit: () => {
      setDisabled(prev => ({ ...prev, step: false, reset: false }));
    },
    onSuccess: () => {
      // clearHighlights();
      setDisabled(prev => ({ ...prev, back: false, reset: false }));
    }
  });
  const [disabled, setDisabled] = useState({
    debug: false,
    reset: true,
    back: true,
    step: true,
  });
  
  const [ darkMode, setDarkMode ] = useState(false);
  const svgRef = useRef();
  const scrollRef = useRef(null);

  const handleInit = async () => {
    const success = await init(codeText);
    if (success) {
      setDisabled({debug: true, reset: false, back: true, step: false});
      setFrozen(true);
    }
  };
  
  const handleStep = async () => {
    const [success, isDone] = await step();
    if (isDone) { // no more reductions
      setDisabled({debug: false, reset: false, back: false, step: true});
    } else if (success) { // success but more reductions
      setDisabled(prev => ({...prev, back: false}));
    }
  };

  const handleBack = async () => {
    const [_, isLast] = await back();
    if (isLast) {  // TODO: Change this to isStart on both sides
      setDisabled(prev => ({...prev, back: true}));
    }
  }

  const handleReset = async () => {
    const success = await reset();  
    if (success) {
      setDisabled({debug: false, reset: true, back: true, step: true});
      setFrozen(false);
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
          <div className="editor-area">
            <CodeEditor codeText={codeText} setCodeText={setCodeText} isFrozen={isFrozen} isDark={darkMode}/>
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
              <TreeCanvas ref={svgRef} />
            </div>
          </Scrollbar>
        </div>
      </Resizable>
    </div>
  );
}

export default App;
