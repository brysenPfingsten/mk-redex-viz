import React, { useState, useEffect } from 'react';

const Resizable = ({ children }) => {
  const [isDragging, setIsDragging] = useState(false);
  const [leftWidth, setLeftWidth] = useState(30);

  useEffect(() => {
    const handleMouseMove = (e) => {
      if (!isDragging) return;

      const container = document.querySelector('.container');
      if (!container) return;

      const rect = container.getBoundingClientRect();
      const newWidth = ((e.clientX - rect.left) / rect.width) * 100;
      setLeftWidth(Math.min(Math.max(newWidth, 20), 70));
    };

    const handleMouseUp = () => {
      setIsDragging(false);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };

    if (isDragging) {
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
    }

    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
    };
  }, [isDragging]);

  return (
    <div className="resizable-container">
      <div 
        className="input-container" 
        style={{ flex: `0 0 ${leftWidth}%` }}
      >
        {children[0]}
      </div>
      
      <div 
        className="resize-handle"
        onMouseDown={(e) => {
          e.preventDefault();
          setIsDragging(true);
        }}
      />
      
      <div 
        className="right-pane" 
        style={{ flex: `0 0 ${100 - leftWidth}%` }}
      >
        {children[1]}
      </div>
    </div>
  );
};


export default Resizable;