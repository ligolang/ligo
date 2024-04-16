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

/** The result of running the command for compiling a contract. */
export type CompileContractResult = {
  /** The entry-point chosen by the user. */
  entrypoint: string,

  /** The format (text, hex, json) for the output chosen by the user. */
  format: string,

  /** The actual result printed by the compiler. */
  result: string
}

/** The result of running the command for compiling a storage. */
export type CompileStorageResult = {
  /** The entry-point chosen by the user. */
  entrypoint: string,

  /** The format (text, hex, json) for the output chosen by the user. */
  format: string,

  /** The value of the storage inputted by the user. */
  storage: string,

  /** The actual result printed by the compiler. */
  result: string
}

/**
 * A helper for appending a project root switch to the compiler command input.
 */
const withProjectRootFlag = (args: string[]) => (projectRootDirectory: Maybe<string>) => {
  if (projectRootDirectory) {
    return args.concat(['--project-root', projectRootDirectory])
  }
  return args
}

/**
 * Initializes the debugger language server state. This is needed since the
 * debugger provides validation mechanisms for user input in the UI.
 *
 * @returns The list of entry-points.
 */
async function prepareState(client: LigoProtocolClient): Promise<[string, string][]> {
  const binaryPath = getBinaryPath(ligoBinaryInfo)
  await client.sendMsg('initializeLanguageServerState', { binaryPath });
  return (await client.sendMsg('setProgramPath', { program: getLastContractPath().path })).body.moduleNames.reverse();
}

/**
 * Opens a quick pick asking the user to input the module name to be used. Uses
 * the debugger for validation.
 */
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

/**
 * The type of a function used to validate the input type.
 *
 * @param inputType Whether the input is for a parameter or storage value.
 * @param valueLang Whether the input value is written in LIGO or Michelson
 * syntax.
 * @param value The inputted value.
 * @returns A promise resolving to an error message, if validation failed.
 */
type ValidateInputType =
  (inputType: InputBoxType, valueLang: InputValueLang)
    => (value: string)
      => Promise<Maybe<string>>;

/**
 * Fetches the contract metadata for the given entry-points and prepares a
 * function that queries the debugger for validation.
 */
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

/**
 * Opens an input box asking the user to provide an input for a parameter.
 *
 * @param validateInput Validation function obtained from
 * {@link prepareParameterStorageValidatorAndContractMetadata}.
 * @param entrypoint The chosen entry-point to which we are getting the
 * parameter.
 * @returns A promise resolving to the inputed parameter value.
 */
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

/**
 * Opens an input box asking the user to provide an input for a storage.
 *
 * @param validateInput Validation function obtained from
 * {@link prepareParameterStorageValidatorAndContractMetadata}.
 * @param entrypoint The chosen entry-point to which we are getting the storage.
 * @returns A promise resolving to the inputed storage value.
 */
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

/**
 * Calls the LIGO compiler with `compile contract` on the provided parameters.
 *
 * @param maybeEntrypoint The entry-point to which we are getting the storage.
 * Asks the user for input if not provided.
 * @param format The format (text, hex, json) for the output chosen by the user.
 * @param showOutput Whether to show the command's result in the LIGO Compiler
 * output channel or not.
 * @returns The result of compiling the contract from LIGO.
 */
/* eslint-disable no-param-reassign */
export async function executeCompileContract(
  context: LigoContext,
  client: LigoProtocolClient,
  maybeEntrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  showOutput = true,
): Promise<CompileContractResult> {
  const entrypoints = await prepareState(client);

  var entrypoint: string;
  if (isDefined(maybeEntrypoint)) {
    entrypoint = maybeEntrypoint
  } else {
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
      ['-m', entrypoint],
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

/**
 * Calls the LIGO compiler with `compile storage` on the provided parameters.
 *
 * @param maybeEntrypoint The entry-point to which we are getting the storage.
 * Asks the user for input if not provided.
 * @param format The format (text, hex, json) for the output chosen by the user.
 * @param maybeStorage The storage to be compiled. Asks the user for input if
 * not provided.
 * @param showOutput Whether to show the command's result in the LIGO Compiler
 * output channel or not.
 * @returns The result of compiling the storage from LIGO.
 */
/* eslint-disable no-param-reassign */
export async function executeCompileStorage(
  context: LigoContext,
  client: LigoProtocolClient,
  maybeEntrypoint: Maybe<string> = undefined,
  format: Maybe<string> = 'text',
  maybeStorage: Maybe<string> = undefined,
  showOutput = true,
): Promise<CompileStorageResult> {
  const entrypoints = await prepareState(client);

  var entrypoint: string;
  if (isDefined(maybeEntrypoint)) {
    entrypoint = maybeEntrypoint
  } else {
    entrypoint = await getEntrypoint(context, client, entrypoints);
  }

  var storage: string;
  if (!isDefined(maybeStorage)) {
    const [validateInput, contractMetadata] =
      await prepareParameterStorageValidatorAndContractMetadata(client, entrypoint);

    storage = await getStorage(context, validateInput, entrypoint, contractMetadata);
  } else {
    storage = maybeStorage
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
      ['-m', entrypoint],
      ['--michelson-format', format],
      ['--allow-json-download']
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

/**
 * Calls the LIGO compiler with `compile expression` on user-provided input.
 *
 * @returns The result of compiling the expression from LIGO.
 */
export async function executeCompileExpression(): Promise<string> {
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

/**
 * Calls the LIGO compiler with `run dry-run` on user-provided input.
 *
 * @returns The result of dry-running an entry-point from LIGO.
 */
export async function executeDryRun(context: LigoContext, client: LigoProtocolClient): Promise<string> {
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
      ['-m', entrypoint],
    ].flat()),
  )
}

/**
 * Calls the LIGO compiler with `run evaluate-call` on user-provided input.
 *
 * @returns The result of evaluating a a function call from LIGO.
 */
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
