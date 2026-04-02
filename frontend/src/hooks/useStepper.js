import { useState } from 'react';
import { buildInitOptions } from '../utils/source_defaults.js';
import {
  parseStepperPayload,
  readStepperHeaders,
  responseErrorMessage,
  thrownErrorMessage,
} from '../utils/stepper_protocol.js';

export default function useStepper({ onSuccess = () => {} } = {}) {
  const initialTree = { name: "Empty", children: [] };
  const [tree, setTree] = useState(initialTree);
  const [stepInfo, setStep] = useState({ step: 0, stepName: '' });

  const send = async (method, url, payload) => {
    let response;
    try {
      response = await fetch(url, {
        method,
        headers: { 'Content-Type': 'application/json' },
        ...(payload && { body: JSON.stringify(payload) }),
        credentials: "include",
      });

      const headers = readStepperHeaders(response);
      const payloadText = await response.text();
      const data = parseStepperPayload(payloadText);

      if (!response.ok) {
        return {
          success: false,
          error: responseErrorMessage(response, data),
          headers,
        };
      }

      if (data != null) {
        setStep({ step: data.step, stepName: data.stepName });
        setTree(JSON.parse(data.program));
      }

      return {
        success: true,
        prog: data?.htmlGuids ?? '',
        headers,
      };
    } catch (error) {
      console.error(error);
      return {
        success: false,
        error: thrownErrorMessage(error),
        headers: readStepperHeaders(response),
      };
    }
  };

  return {
    tree,
    stepInfo,
    init: async (codeText, sourceMode, compileProfile, searchStrategy) => {
      const result = await send(
        'POST',
        '/api/post/init',
        buildInitOptions(codeText, sourceMode, compileProfile, searchStrategy),
      );
      if (result.success) onSuccess();
      if (!result.success) return [false, result.error];
      return [true, result.prog];
    },
    step: async () => {
      const result = await send('GET', '/api/get/next');
      if (result.success) onSuccess();
      return [result.success, result.headers.isDone, result.error];
    },
    back: async () => {
      const result = await send('POST', '/api/post/back');
      if (result.success) onSuccess();
      return [result.success, result.headers.isAtStart, result.error];
    },
    reset: async () => {
      const result = await send('POST', '/api/post/reset');
      if (result.success) onSuccess();
      return [result.success, result.error];
    }
  };
}
