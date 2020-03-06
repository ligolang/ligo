import axios from 'axios';

import { AppState } from '../redux/app';
import { Language } from '../redux/types';

export async function getExample(id: string) {
  const response = await axios.get(`/static/examples/${id}`);
  return response.data;
}

export async function compileContract(
  syntax: Language,
  code: string,
  entrypoint: string,
  format?: string
) {
  const response = await axios.post('/api/compile-contract', {
    syntax,
    code,
    entrypoint,
    format
  });
  return response.data;
}

export async function compileExpression(
  syntax: Language,
  expression: string,
  format?: string
) {
  const response = await axios.post('/api/compile-expression', {
    syntax,
    expression: `${expression}`,
    format
  });
  return response.data;
}

export async function compileStorage(
  syntax: Language,
  code: string,
  entrypoint: string,
  storage: string,
  format?: string
) {
  const response = await axios.post('/api/compile-storage', {
    syntax,
    code,
    entrypoint,
    storage,
    format
  });
  return response.data;
}

export async function dryRun(
  syntax: Language,
  code: string,
  entrypoint: string,
  parameters: string,
  storage: string
) {
  // For whatever reason, storage set by examples is not treated as a string. So we convert it here.
  storage = `${storage}`;

  const response = await axios.post('/api/dry-run', {
    syntax,
    code,
    entrypoint,
    parameters,
    storage
  });
  return response.data;
}

export async function share({
  editor,
  compile,
  dryRun,
  deploy,
  evaluateValue,
  evaluateFunction
}: Partial<AppState>) {
  const params = {
    editor,
    compile,
    dryRun,
    deploy,
    evaluateValue,
    evaluateFunction
  };

  // We don't want to store the following configuration
  if (params.compile) {
    delete params.compile.michelsonFormat;
  }
  if (params.deploy) {
    delete params.deploy.useTezBridge;
  }

  const response = await axios.post('/api/share', params);
  return response.data;
}

export async function deploy(
  syntax: Language,
  code: string,
  entrypoint: string,
  storage: string
) {
  // For whatever reason, storage set by examples is not treated as a string. So we convert it here.
  storage = `${storage}`;

  const response = await axios.post('/api/deploy', {
    syntax,
    code,
    entrypoint,
    storage
  });
  return response.data;
}

export async function evaluateValue(
  syntax: Language,
  code: string,
  entrypoint: string
) {
  const response = await axios.post('/api/evaluate-value', {
    syntax,
    code,
    entrypoint
  });
  return response.data;
}

export async function runFunction(
  syntax: Language,
  code: string,
  entrypoint: string,
  parameters: string
) {
  const response = await axios.post('/api/run-function', {
    syntax,
    code,
    entrypoint,
    parameters
  });
  return response.data;
}

export function getErrorMessage(ex: any): string {
  if (ex.response && ex.response.data) {
    return ex.response.data.error;
  } else if (ex instanceof Error) {
    return ex.message;
  }

  return JSON.stringify(ex);
}
