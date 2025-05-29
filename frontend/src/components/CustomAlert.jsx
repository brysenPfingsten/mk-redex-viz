import React from 'react';
import './CustomAlert.css';

const CustomAlert = ({ isOpen, message, onClose }) => {
  if (!isOpen) return null;

  return (
    <div className="custom-alert">
      <div className="custom-alert-content">
        <p className="custom-alert-message">{message}</p>
        <button className="custom-alert-button" onClick={onClose}>
          OK
        </button>
      </div>
    </div>
  );
};

export default CustomAlert;
