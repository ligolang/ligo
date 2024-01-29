import { createRememberingInputBox, createQuickPickBox } from '../ui'
import {
  CommandRequiredArguments, executeCommand, getLastContractPath,
} from './common';
import * as ex from '../../common/exceptions'
import { InputBoxType, InputValueLang, isDefined, Maybe } from '../../common/base';
import { LigoContext } from '../../common/LigoContext';
import { LigoProtocolClient } from '../../common/LigoProtocolClient';

import { getModuleName, getParameterOrStorage } from '../../debugger/ui';
import { ContractMetadata } from '../../debugger/base';
import { ValidateValueCategory } from '../../debugger/messages';
import { getBinaryPath, ligoBinaryInfo } from '../../common/config';

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

async function prepareState(client: LigoProtocolClient): Promise<[string, string][]> {
  const binaryPath = getBinaryPath(ligoBinaryInfo)
  await client.sendMsg('initializeLanguageServerState', { binaryPath });
  return (await client.sendMsg('setProgramPath', { program: getLastContractPath().path })).body.moduleNames.reverse();
}

async function getEntrypoint(
  context: LigoContext,
  client: LigoProtocolClient,
  entrypoints: [string, string][]
): Promise<string> {
  return getModuleName(
    context,
    async (moduleName) => {
      return (await client.sendMsg('validateModuleName', { moduleName })).body?.errorMessage;
    },
    entrypoints
  );
}

type ValidateInputType =
  (inputType: InputBoxType, valueLang: InputValueLang)
    => (value: string)
      => Promise<Maybe<string>>;

async function prepareParameterStorageValidatorAndContractMetadata(
  client: LigoProtocolClient,
  entrypoint: string
): Promise<[ValidateInputType, ContractMetadata]> {
  const contractMetadata: ContractMetadata =
    (await client.sendMsg('getContractMetadata', { moduleName: entrypoint })).body;

  // It will resolve into default one
  const pickedEntrypoint = "";
  await client.sendMsg('validateEntrypoint', { pickedEntrypoint });

  const validateInput =
    (category: ValidateValueCategory, valueLang: InputValueLang) => async (value: string): Promise<Maybe<string>> => {
      return (
        await client.sendMsg(
          'validateValue',
          { value, category, valueLang }
        )
      ).body?.errorMessage;
    }

  return [validateInput, contractMetadata];
}

async function getParameter(
  context: LigoContext,
  validateInput: ValidateInputType,
  entrypoint: string,
  contractMetadata: ContractMetadata
): Promise<string> {
  const pickedEntrypoint = "";

  return (await getParameterOrStorage(
    context,
    validateInput,
    "parameter",
    "Input the contract parameter",
    "Parameter value",
    entrypoint,
    contractMetadata,
    pickedEntrypoint,
    false
  ))[0];
}

async function getStorage(
  context: LigoContext,
  validateInput: ValidateInputType,
  entrypoint: string,
  contractMetadata: ContractMetadata
): Promise<string> {
  const pickedEntrypoint = "";

  return (await getParameterOrStorage(
    context,
    validateInput,
    "storage",
    "Input the contract storage",
    "Storage value",
    entrypoint,
    contractMetadata,
    pickedEntrypoint,
    false
  ))[0];
}

/* eslint-disable no-param-reassign */
export async function executeCompileContract(
  context: LigoContext,
  client: LigoProtocolClient,
  entrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  showOutput = true,
): Promise<CompileContractResult> {
  const entrypoints = await prepareState(client);

  if (!isDefined(entrypoint)) {
    entrypoint = await getEntrypoint(context, client, entrypoints);
  }

  const formats = ['text', 'hex', 'json']
  if (!format) {
    format = await createQuickPickBox(formats, 'Format', 'Compilation format')
  }

  if (!formats.includes(format)) {
    throw new ex.InvalidChoiceException(format, formats)
  }

  const result = await executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag([
      ['compile', 'contract', path],
      Boolean(entrypoint) ? ['-m', entrypoint] : [],
      ['--michelson-format', format],
    ].flat()),
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
  context: LigoContext,
  client: LigoProtocolClient,
  entrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  storage: Maybe<string> = undefined,
  showOutput = true,
): Promise<CompileStorageResult> {
  const entrypoints = await prepareState(client);

  if (!isDefined(entrypoint)) {
    entrypoint = await getEntrypoint(context, client, entrypoints);
    if (!isDefined(entrypoint)) {
      throw new ex.UserInterruptionException()
    }
  }

  if (!isDefined(storage)) {
    const [validateInput, contractMetadata] =
      await prepareParameterStorageValidatorAndContractMetadata(client, entrypoint);

    storage = await getStorage(context, validateInput, entrypoint, contractMetadata);
  }

  if (!format) {
    format = await createQuickPickBox(['text', 'hex', 'json'], 'Format', 'Compilation format')
  }
  if (format !== 'text' && format !== 'hex' && format !== 'json') {
    throw new ex.InvalidChoiceException(format, ['text', 'hex', 'json'])
  }

  const result = await executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag([
      ['compile', 'storage', path, storage],
      Boolean(entrypoint) ? ['-m', entrypoint] : [],
      ['--michelson-format', format],
    ].flat()),
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

export async function executeCompileExpression() {
  const declarations = await executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag([
      ['info', 'list-declarations', path],
      ['--format', 'json'],
    ].flat()),
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
    (path: string) => (syntax: string) => withProjectRootFlag([
      ['compile', 'expression', syntax, maybeExpression],
      ['--init-file', path],
    ].flat()),
    CommandRequiredArguments.Path
    | CommandRequiredArguments.Ext
    | CommandRequiredArguments.ProjectRoot,
  )
}

export async function executeDryRun(context: LigoContext, client: LigoProtocolClient) {
  const entrypoints = await prepareState(client);
  const entrypoint = await getEntrypoint(context, client, entrypoints);

  const [validateInput, contractMetadata] =
    await prepareParameterStorageValidatorAndContractMetadata(client, entrypoint);

  const parameter = await getParameter(context, validateInput, entrypoint, contractMetadata);
  const storage = await getStorage(context, validateInput, entrypoint, contractMetadata);

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag([
      ['run', 'dry-run', path, parameter, storage],
      Boolean(entrypoint) ? ['-m', entrypoint] : [],
    ].flat()),
  )
}

export async function executeEvaluateFunction(context: LigoContext) {
  const maybeEntrypoint = await createRememberingInputBox(context, {
    title: 'Function',
    placeHolder: 'Enter function to evaluate',
    rememberingKey: 'call-expression',
  });
  const maybeExpr = await createRememberingInputBox(context, {
    title: 'Parameter',
    placeHolder: 'Parameter expression',
    rememberingKey: 'call-arg',
  })

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag([
      ['run', 'evaluate-call', path, maybeEntrypoint, maybeExpr],
    ].flat()),
  )
}

export async function executeEvaluateValue(context: LigoContext) {
  const maybeValue = await createRememberingInputBox(context, {
    title: 'Value',
    placeHolder: 'Enter value to evaluate',
    rememberingKey: 'call-value',
  });

  return executeCommand(
    ligoBinaryInfo,
    (path: string) => withProjectRootFlag([
      ['run', 'evaluate-expr', path, maybeValue],
    ].flat()),
  )
}
