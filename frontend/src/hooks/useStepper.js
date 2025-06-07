import { useState, React } from 'react';

export default function useStepper({ onSuccess = () => {}} = {}) {
  const initialTree = { name: "Empty", children: [] };
  const [tree, setTree] = useState(initialTree);
  const [stepInfo, setStep] = useState({ step: 0, stepName: '' });
  
  const send = async (method, url, body) => {
    let res;
    try {
      res = await fetch(url, {
        method,
        headers: { 'Content-Type': 'application/json' },
        ...(body && { body: JSON.stringify({ text: body }) }),
      });
      
      const text = await res.text();
      let data;
      try {
        data = JSON.parse(text);
      } catch (err) {
        throw new Error(`Failed to parse response: ${err.message}`);
      }

      if (!res.ok) {
        if (res.status === 400 && data.error) {
          return {
            success: false,
            error: data.error,
    headers: {}, 
          };
        }
        throw new Error(`HTTP error! Status: ${res.status}`);
      }

      setStep({ step: data.step, stepName: data.stepName });
      setTree(JSON.parse(data.program));
      return {
        success: true,
        prog: data.htmlGuids ? data.htmlGuids : '',
        headers: {
          isLast: res.headers.get('X-Is-Last') === 'true',
          isDone: res.headers.get('X-Done') === 'true',
        },
      };
    } catch (err) {
      console.error(err);
      return {
        success: false,
        headers: {
          isLast: res ? res.headers.get('X-Is-Last') === 'true' : false,
          isDone: res ? res.headers.get('X-Done') === 'true' : false,
        },
      };
    }
  };
  
  return {
    tree,
    stepInfo,
    init: async (codeText) => {
      const result = await send('POST', '/api/post/init', codeText);
      if (result.success) onSuccess();
      if (!result.success) return [result.success, result.error];
      return [result.success, result.prog];
    },
    step: async () => {
      const result = await send('GET', '/api/get/next');
      if (result.success) onSuccess();
      return [result.success, result.headers.isDone];
    },
    back: async () => {
      const result = await send('POST', '/api/post/back');
      if (result.success) onSuccess;
      return [result.success, result.headers.isLast];
    },
    reset: async () => {
      const result = await send('POST', '/api/post/reset');
      if (result.success) onSuccess;
      return result.success;
    }
  };
}

