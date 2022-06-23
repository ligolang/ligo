import { LanguageClient } from 'vscode-languageclient/node'
import { createRememberingInputBox, createQuickPickBox, Maybe } from '../ui'
import { CommandRequiredArguments, executeCommand } from './common';

type CompileContractResult = {
  entrypoint: string,
  format: string,
  result: string
}

type CompileStorageResult = {
  entrypoint: string,
  format: string,
  storage : string,
  result: string
}

export type SilentCompilationOptions = {
  entrypoint: string,
  onPath: Maybe<string>,
  flags: string[]
}

const ligoBinaryInfo = { name: 'ligo', path: 'ligoLanguageServer.ligoBinaryPath' }

export async function executeSilentCompileContract(
  client: LanguageClient,
  options: SilentCompilationOptions,
) {
  let args = ['compile', 'contract', '-e', options.entrypoint].concat(options.flags)
  if (options.onPath) {
    args = args.concat(['--output-file', options.onPath])
  }

  return executeCommand(
    ligoBinaryInfo,
    (path) => args.concat([path]),
    client,
    CommandRequiredArguments.Path,
    options.onPath === undefined,
  )
}

/* eslint-disable no-param-reassign */
export async function executeCompileContract(
  client: LanguageClient,
  entrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  showOutput = true,
  errorPrefix = undefined,
) : Promise<CompileContractResult> {
  if (!entrypoint) {
    entrypoint = await createRememberingInputBox({
      title: 'Entrypoint',
      placeHolder: 'Enter entrypoint to compile',
      rememberingKey: 'compile-contract',
      defaultValue: 'main',
    });
  }
  if (!format) {
    format = await createQuickPickBox(['text', 'hex', 'json'], 'Format', 'Compilation format')
  } if (format !== 'text' && format !== 'hex' && format !== 'json') {
    console.error('Invalid compiling format');
    return undefined;
  }
  if (!entrypoint) {
    return undefined;
  }
  const result = await executeCommand(
    ligoBinaryInfo,
    (path: string) => [
      'compile',
      'contract',
      path,
      '-e',
      entrypoint,
      '--michelson-format',
      format,
    ],
    client,
    CommandRequiredArguments.Path,
    showOutput,
    errorPrefix,
  )

  return {
    entrypoint,
    format,
    result,
  };
}

/* eslint-disable no-param-reassign */
export async function executeCompileStorage(
  client: LanguageClient,
  entrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  storage: Maybe<string> = undefined,
  showOutput = true,
  errorPrefix = undefined,
) : Promise<CompileStorageResult> {
  if (!entrypoint) {
    entrypoint = await createRememberingInputBox({
      title: 'Entrypoint',
      placeHolder: 'Enter entrypoint to compile',
      rememberingKey: 'storage-entrypoint',
      defaultValue: 'main',
    });
  }
  if (!storage) {
    storage = await createRememberingInputBox({
      title: 'Storage expression',
      placeHolder: 'Enter storage expression',
      rememberingKey: 'storage-expression',
      defaultValue: '',
    });
  }
  if (!format) {
    format = await createQuickPickBox(['text', 'hex', 'json'], 'Format', 'Compilation format')
  } if (format !== 'text' && format !== 'hex' && format !== 'json') {
    console.error('Invalid compiling format');
    return undefined;
  }
  if (!entrypoint || !storage) {
    return undefined;
  }
  const result = await executeCommand(
    ligoBinaryInfo,
    (path: string) => [
      'compile',
      'storage',
      path,
      storage,
      '-e',
      entrypoint,
      '--michelson-format',
      format,
    ],
    client,
    CommandRequiredArguments.Path,
    showOutput,
    errorPrefix,
  )

  return {
    entrypoint,
    format,
    storage,
    result,
  };
}

export async function executeCompileExpression(client: LanguageClient) {
  const listOfExpressions = await executeCommand(
    ligoBinaryInfo,
    (path : string) => ['info', 'list-declarations', path],
    client,
    1,
    false,
  )
  const exp = listOfExpressions.toString().split(':')[1].replace(/\s+/g, ' ').split(' ').slice(1, -1);
  const maybeExpression = await createQuickPickBox(exp, 'Expressions', 'Possible expressions for this contract')

  if (!maybeExpression) {
    return undefined;
  }
  return executeCommand(
    ligoBinaryInfo,
    (path: string, syntax: string) => [
      'compile',
      'expression',
      syntax,
      maybeExpression,
      '--init-file',
      path,
    ],
    client,
    CommandRequiredArguments.PathAndExt,
  )
}

export async function executeDryRun(client: LanguageClient) {
  const maybeParameter = await createRememberingInputBox({
    title: 'Parameter',
    placeHolder: 'Entrypoint parameter',
    rememberingKey: 'dry-run-parameter',
    defaultValue: undefined,
  })
  const maybeStorage = await createRememberingInputBox({
    title: 'Storage',
    placeHolder: 'Entrypoint storage',
    rememberingKey: 'dry-run-storage',
    defaultValue: undefined,
  })
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter entrypoint to compile',
    rememberingKey: 'dry-run-entrypoint',
    defaultValue: 'main',
  });
  if (!maybeParameter || !maybeStorage || !maybeEntrypoint) {
    return undefined;
  }
  return executeCommand(
    ligoBinaryInfo,
    (path: string) => ['run', 'dry-run', path, maybeParameter, maybeStorage, '-e', maybeEntrypoint],
    client,
  )
}

export async function executeEvaluateFunction(client: LanguageClient) {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter function to compile',
    rememberingKey: 'call-entrypoint',
    defaultValue: 'main',
  });
  const maybeExpr = await createRememberingInputBox({
    title: 'Arguments',
    placeHolder: 'Function arguments',
    rememberingKey: 'call-arg',
    defaultValue: undefined,
  })
  if (!maybeExpr || !maybeEntrypoint) {
    return undefined;
  }
  return executeCommand(
    ligoBinaryInfo,
    (path: string) => ['run', 'evaluate-call', path, maybeExpr, '-e', maybeEntrypoint],
    client,
  )
}

export async function executeEvaluateValue(client: LanguageClient) {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter an value or a function to compile',
    rememberingKey: 'call-value',
    defaultValue: '',
  });
  if (!maybeEntrypoint) {
    return undefined;
  }
  return executeCommand(
    ligoBinaryInfo,
    (path: string) => ['run', 'evaluate-expr', path, '-e', maybeEntrypoint],
    client,
  )
}
