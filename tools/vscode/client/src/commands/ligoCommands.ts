import * as vscode from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node'
import { createRememberingInputBox, createQuickPickBox } from '../ui'
import {
  CommandRequiredArguments, executeCommand, ligoBinaryInfo
} from './common';
import { Maybe } from '../common'
import * as ex from '../exceptions'

/* eslint-disable no-bitwise */

export type CompileContractResult = {
  entrypoint: string,
  format: string,
  result: string
}

export type CompileStorageResult = {
  entrypoint: string,
  format: string,
  storage: string,
  result: string
}

export type SilentCompilationOptions = {
  entrypoint: string,
  printToConsole: boolean,
  onPath: Maybe<string>,
  flags: string[]
}

const withProjectRootFlag = (args: string[]) => (projectRootDirectory: Maybe<string>) => {
  if (projectRootDirectory) {
    return args.concat(['--project-root', projectRootDirectory])
  }
  return args
}

const withDeprecated = (args: string[]) => {
  const config = vscode.workspace.getConfiguration()
  const deprecated: boolean = config.get('ligoLanguageServer.deprecated')
  if (deprecated) {
    return args.concat('--deprecated')
  }
  return args
}

/* eslint-disable no-param-reassign */
export async function executeCompileContract(
  client: LanguageClient,
  entrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  showOutput = true,
): Promise<CompileContractResult> {
  if (entrypoint === null || entrypoint === undefined) {
    entrypoint = await createRememberingInputBox({
      title: 'Entrypoint',
      placeHolder: 'Enter entrypoint to compile',
      rememberingKey: 'compile-contract',
      defaultValue: '',
    });
  }

  if (!format) {
    format = await createQuickPickBox(['text', 'hex', 'json'], 'Format', 'Compilation format')
  }

  if (format !== 'text' && format !== 'hex' && format !== 'json') {
    throw new ex.InvalidChoiceException(format, ['text', 'hex', 'json'])
  }

  const result = await executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag(withDeprecated([
      'compile',
      'contract',
      path,
      '-m',
      entrypoint,
      '--michelson-format',
      format,
    ])),
    client,
    CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
    showOutput,
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
): Promise<CompileStorageResult> {
  if (entrypoint === null || entrypoint === undefined) {
    entrypoint = await createRememberingInputBox({
      title: 'Entrypoint',
      placeHolder: 'Enter entrypoint to compile',
      rememberingKey: 'storage-entrypoint',
      defaultValue: '',
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
  }
  if (format !== 'text' && format !== 'hex' && format !== 'json') {
    throw new ex.InvalidChoiceException(format, ['text', 'hex', 'json'])
  }

  const result = await executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag(withDeprecated([
      'compile',
      'storage',
      path,
      storage,
      '-m',
      entrypoint,
      '--michelson-format',
      format,
    ])),
    client,
    CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
    showOutput,
  )

  return {
    entrypoint,
    format,
    storage,
    result,
  };
}

export async function executeCompileExpression(client: LanguageClient) {
  const declarations = await executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag(withDeprecated([
      'info',
      'list-declarations',
      path,
      '--format',
      'json',
    ])),
    client,
    CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
    false,
  )

  const listOfExpressions: string[] = JSON.parse(declarations).declarations
  const maybeExpression = await createQuickPickBox(
    listOfExpressions,
    'Expressions',
    'Possible expressions for this contract',
  )

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => (syntax: string) => withProjectRootFlag(withDeprecated([
      'compile',
      'expression',
      syntax,
      maybeExpression,
      '--init-file',
      path,
    ])),
    client,
    CommandRequiredArguments.Path
    | CommandRequiredArguments.Ext
    | CommandRequiredArguments.ProjectRoot,
  )
}

export async function executeDryRun(client: LanguageClient) {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter entrypoint to compile',
    rememberingKey: 'dry-run-entrypoint',
    defaultValue: '',
  });
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

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag(withDeprecated([
      'run',
      'dry-run',
      path,
      maybeParameter,
      maybeStorage,
      '-m',
      maybeEntrypoint,
    ])),
    client,
  )
}

export async function executeEvaluateFunction(client: LanguageClient) {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Function',
    placeHolder: 'Enter function to evaluate',
    rememberingKey: 'evaluate-function',
    defaultValue: 'main',
  });
  const maybeExpr = await createRememberingInputBox({
    title: 'Parameter',
    placeHolder: 'Parameter expression',
    rememberingKey: 'param-expr',
    defaultValue: undefined,
  })

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag(withDeprecated([
      'run',
      'evaluate-call',
      path,
      maybeEntrypoint,
      maybeExpr,
    ])),
    client,
  )
}

export async function executeEvaluateValue(client: LanguageClient) {
  const maybeValue = await createRememberingInputBox({
    title: 'Value',
    placeHolder: 'Enter value to evaluate',
    rememberingKey: 'call-value',
    defaultValue: 'main',
  });

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag(withDeprecated([
      'run',
      'evaluate-expr',
      path,
      maybeValue,
    ])),
    client,
  )
}
