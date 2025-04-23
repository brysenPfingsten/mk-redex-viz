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
  const { stepInfo, tree, init, step, back, reset, loading } = useStepper();
  const [ darkMode, setDarkMode ] = useState(false);
  const svgRef = useRef();

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
            <CodeEditor {...stepInfo} onInit={init} />
          </div>
          <Toolbar
            onDebug={init}
            onStep={step}
            onBack={back}
            onReset={reset}
            disabled={loading}
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
