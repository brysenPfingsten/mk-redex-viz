import { useState, React } from 'react';

export default function useStepper({ onSuccess = () => {}, onInit = () => {} } = {}) {
  const initialTree = { name: "Empty", children: [] };
  const [tree, setTree] = useState(initialTree);
  const [stepInfo, setStep] = useState({ step: 0, stepName: '' });
  const [loading, setLoading] = useState(false);

  const send = async (method, url, body) => {
    setLoading(true);
    const res = await fetch(url, {
      method,
      headers: { 'Content-Type': 'application/json' },
      ...(body && { body: JSON.stringify({ text: body }) }),
    });

    const text = await res.text();
    const data = JSON.parse(text);
    setStep({ step: data.step, stepName: data.stepName });
    setTree(JSON.parse(data.program));
    setLoading(false);
    return true;
  };

  return {
    tree,
    stepInfo,
    loading,
    init: async (codeText) => {
      const success = await send('POST', '/api/post/init', codeText);
      if (success) onInit();
    },
    step: async () => {
      const success = await send('GET', '/api/get/next');
      if (success) onSuccess();
    },
    back: () => send('POST', '/api/post/back'),
    reset: () => send('POST', '/api/post/reset'),
  };
}

