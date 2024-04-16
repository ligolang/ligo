import * as os from 'os'
import * as vscode from 'vscode'
import { extname, join, dirname } from 'path';
import { existsSync } from 'fs'
import { execFile, execFileSync } from 'child_process';
import { promisify } from 'util';

import { extensions } from '../common'
import { Maybe } from '../../common/base';

import * as ex from '../../common/exceptions'
import { BinaryInfo, getBinaryPath } from '../../common/config';

/** An output channel to write about the output of the LIGO compiler. */
export const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')

/* eslint-disable no-bitwise */

/** A reference to the last contract path that the extension has remembered. */
let lastContractPath: string;

/** Changes the last contract path that the extension has remembered. */
export function changeLastContractPath(newPath: string) {
  lastContractPath = newPath
}

/**
 * LIGO sometimes expects the dialect name in a command, such as "cameligo" or
 * "jsligo". This function returns such name, given a file extension.
 */
function extToDialect(ext: string) {
  switch (ext) {
    case '.mligo': return 'cameligo'
    case '.jsligo': return 'jsligo'
    default:
      throw new ex.UnknownLigoDialectExtensionException(ext)
  }
}

/* eslint-disable no-shadow */
/* eslint-disable no-unused-vars */
/* eslint-disable no-multi-spaces */
/**
 * Some commands may require some extra input to be provided to LIGO, such as
 * the path to the contract, its syntax name, or a path to the project root.
 * This enum indicates all required inputs. Meant to be used as flags.
 */
export enum CommandRequiredArguments {
  /** Indicates that no arguments are required. */
  NoArgs = 0,

  /** Indicates that the path to the file is required. */
  Path = 1 << 1,

  /** Indicates that the syntax name is required. */
  Ext = 1 << 2,

  /** Indicates that the path to the project root is required. */
  ProjectRoot = 1 << 3,
}

/**
 * An object containing the path to some file and its extracted file extension.
 */
export type ContractFileData = {
  /** The path to some contract. */
  path: string,

  /** The file extension of the contract path. */
  ext: string,
}

/**
 * Gets the file data of the last seen contract (may be the current one if it's
 * currently open).
 */
export function getLastContractPath(): ContractFileData {
  const activeEditor: vscode.TextEditor | undefined = vscode.window.activeTextEditor
  if (!activeEditor) {
    throw new ex.NoContractPathException(undefined)
  }

  let path = activeEditor.document.fileName;
  let ext = extname(path);

  // `vscode.window.activeTextEditor` may return output panels. This is known
  // and won't be fixed, see: https://github.com/Microsoft/vscode/issues/58869
  if (!extensions.includes(ext)) {
    if (!lastContractPath) {
      throw new ex.NoContractPathException(path)
    }

    path = lastContractPath;
    ext = extname(lastContractPath);
  }

  lastContractPath = path;
  return { path, ext }
}

/** The LIGO project file name (`ligo.json`), which marks the project root. */
const packageName = 'ligo.json'

/**
 * Attemps to find a file called {@link packageName}, searching from the
 * provided directory name to all parent directories, stopping at the first
 * found occurrence (if any). */
function findPackage(dirname: string): Maybe<string> {
  if (dirname === '') {
    return undefined;
  }
  if (existsSync(join(dirname, packageName))) {
    return dirname;
  }
  return findPackage(dirname.substring(0, dirname.lastIndexOf('/')));
}

/**
 * Prefixes each line of a string with a specified prefix.
 * @param s The input string.
 * @param p The prefix to be added to each line.
 * @returns The string with each line prefixed by the specified prefix.
 */
function prefixLines(s: string, p: string): string {
  if (s.trim().length == 0) {
    return s
  }
  return s.split('\n').map(l => `${p}${l}`).join("\n")
}

/**
 * Executes the provided command using the given binary information.
 *
 * @param binary The name and path to a binary to be executed.
 * @param command The command to be executed.
 * @param commandArgs The arguments that need to be provided to the command. See
 * {@link CommandRequiredArguments}.
 * @param showOutput Whether to show the command's result in the
 * {@link ligoOutput} channel or not.
 * @throws ex.UserInterruptionException if the command has an `undefined` part.
 * @throws ex.ExecutionException if the binary failed to run.
 * @returns The formatted result of running the command.
 */
export async function executeCommand(
  binary: BinaryInfo,
  command: any,
  commandArgs = CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
  showOutput = true,
): Promise<string> {
  const contractInfo = getLastContractPath()
  const ligoPath = getBinaryPath(binary);
  const ligoJsonPath = findPackage(dirname(contractInfo.path))

  if (commandArgs & CommandRequiredArguments.Path) {
    command = command(contractInfo.path)
  }
  if (commandArgs & CommandRequiredArguments.Ext) {
    command = command(extToDialect(contractInfo.ext))
  }
  if (commandArgs & CommandRequiredArguments.ProjectRoot) {
    command = command(ligoJsonPath)
  }
  try {
    if (command.includes(undefined)) {
      throw new ex.UserInterruptionException()
    }

    var result = await promisify(execFile)(ligoPath, command, { cwd: ligoJsonPath },)
    if (showOutput) {
      ligoOutput.appendLine(prefixLines(result.stderr, "!> "))
      ligoOutput.appendLine(result.stdout)
      ligoOutput.show();
    }
    return result.stdout;
  } catch (error) {
    throw new ex.ExecutionException(error)
  }
}
