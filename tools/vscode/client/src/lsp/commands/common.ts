import * as os from 'os'
import * as vscode from 'vscode'
import { extname, join, dirname } from 'path';
import { existsSync } from 'fs'
import { execFileSync } from 'child_process';

import { extensions } from '../common'
import { Maybe, isDefined } from '../../common/base';

import * as ex from '../../common/exceptions'
import { BinaryInfo } from '../../common/config';

export const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')

/* eslint-disable no-bitwise */

let lastContractPath: string;

export function changeLastContractPath(newPath: string) {
  lastContractPath = newPath
}

function extToDialect(ext: string) {
  switch (ext) {
    case '.mligo': return 'cameligo'
    case '.jsligo': return 'jsligo'
    default:
      throw new ex.UnknownLigoDialectExtensionException(ext)
  }
}

export function getBinaryPath(info: BinaryInfo) {
  const config = vscode.workspace.getConfiguration()
  let binaryPath = config.get<string>(info.path)
  if (!isDefined(binaryPath)) {
    // We always expect some value because a default value is registered in package.json
    throw new Error("Unexpectedly no LIGO path from config")
  }
  return binaryPath
}

/* eslint-disable no-shadow */
/* eslint-disable no-unused-vars */
/* eslint-disable no-multi-spaces */
export enum CommandRequiredArguments {
  NoArgs = 0,
  Path = 1 << 1,
  Ext = 1 << 2,
  ProjectRoot = 1 << 3,
}

export type ContractFileData = {
  path: string,
  ext: string,
}

export function getLastContractPath() {
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

const packageName = 'ligo.json'

function findPackage(dirname: string): Maybe<string> {
  if (dirname === '') {
    return undefined;
  }
  if (existsSync(join(dirname, packageName))) {
    return dirname;
  }
  return findPackage(dirname.substring(0, dirname.lastIndexOf('/')));
}

export function executeCommand(
  binary: BinaryInfo,
  command: any,
  commandArgs = CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
  showOutput = true,
): string {
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

    const result = execFileSync(ligoPath, command, { cwd: ligoJsonPath }).toString()
    if (showOutput) {
      ligoOutput.appendLine(result)
      ligoOutput.show();
    }
    return result;
  } catch (error) {
    throw new ex.ExecutionException(error)
  }
}
