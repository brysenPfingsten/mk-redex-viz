import { useState, useRef, useEffect } from 'react';
import React from 'react';
import CodeEditor      from './components/CodeEditor';
import Toolbar         from './components/Toolbar';
import StepInfo        from './components/StepInfo';
import TreeCanvas      from './components/TreeCanvas';
import useStepper      from './hooks/useStepper';
import Resizable       from './components/Resizable';
import './styles.css'

function App() {
  const [codeText, setCodeText] = useState('');
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

  const handleInit = async () => {
    const success = await init(codeText);
    if (success) {
      setDisabled({debug: true, reset: false, back: true, step: false});
    }
  };
  
  const handleStep = async () => {
    const [success, isDone] = await step();
    if (isDone) { // no more reductions
      setDisabled({debug: false, reset: false, back: false, step: true});
    } else if (success) {
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
            <CodeEditor codeText={codeText} setCodeText={setCodeText}/>
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
          <div className="scroll-container">
            <TreeCanvas ref={svgRef} />
          </div>
        </div>
      </Resizable>
    </div>
  );
}

export default App;
