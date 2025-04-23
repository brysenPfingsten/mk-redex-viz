import { useState, React } from 'react';

export default function useStepper() {
  const initialTree = { "name" : "Empty", "children" : [] };
  const [tree,   setTree]   = useState(initialTree);
  const [stepInfo, setStep] = useState({ step: 0, stepName: '' });
  const [loading, setLoading] = useState(false);

  const send = async (method, url, body) => {
    setLoading(true);
    const res = await fetch(url, {
      method, headers:{'Content-Type':'application/json'},
      ...(body && { body: JSON.stringify({ text: body }) })
    });
    const text = await res.text();
    const data = JSON.parse(text);
    setStep({ step: data.step, stepName: data.stepName });
    setTree(JSON.parse(data.program));
    setLoading(false);
  };

  return {
    tree, stepInfo, loading,
    init: () => send('POST','/api/post/init'),
    step: () => send('GET','/api/get/next'),
    back: () => send('POST','/api/post/back'),
    reset:() => send('POST','/api/post/reset'),
  };
}
