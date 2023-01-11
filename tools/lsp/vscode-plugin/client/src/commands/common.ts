import * as vscode from 'vscode'
import { LanguageClient, RequestType } from 'vscode-languageclient/node'
import { extname, join, dirname } from 'path';
import { existsSync } from 'fs'
import { execFileSync } from 'child_process';

import { Maybe } from '../ui'

import { extensions } from '../common'

import * as ex from '../exceptions'

export const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')

/* eslint-disable no-bitwise */

let lastContractPath: string;

type BinaryInfo = {
  name: string,
  path: string,
}

export function changeLastContractPath(newPath: string) {
  lastContractPath = newPath
}

function extToDialect(ext: string) {
  switch (ext) {
    case '.pligo':
    case '.ligo': return 'pascaligo'
    case '.mligo': return 'cameligo'
    case '.jsligo': return 'jsligo'
    default:
      throw new ex.UnknownLigoDialectExtensionException(ext)
  }
}

export function getBinaryPath(info: BinaryInfo, config: vscode.WorkspaceConfiguration) {
  let binaryPath = config.get<string>(info.path)
  if (binaryPath) {
    return binaryPath
  }

  try {
    vscode.window.showWarningMessage(`'${info.name}' binary not found through the configuration for the Visual Studio Code extension. Using PATH.`)

    binaryPath = execFileSync('which', [info.name]).toString().trim()

    vscode.window.showWarningMessage(`${info.path} variable was updated to ${binaryPath}`)
    config.update(info.path, binaryPath)
    return binaryPath
  } catch {
    throw new ex.BinaryNotFoundExtension(info.name)
  }
}

/* eslint-disable no-shadow */
/* eslint-disable no-unused-vars */
/* eslint-disable no-multi-spaces */
export enum CommandRequiredArguments {
  NoArgs      = 0,
  Path        = 1 << 1,
  Ext         = 1 << 2,
  ProjectRoot = 1 << 3,
}

export type ContractFileData = {
  path: string,
  ext: string,
}

export function getLastContractPath() {
  if (!vscode.window.activeTextEditor) {
    throw new ex.NoContractPathException(undefined)
  }

  let path = vscode.window.activeTextEditor.document.uri.fsPath;
  const ext = extname(path);

  if (!extensions.includes(ext)) {
    if (!lastContractPath) {
      throw new ex.NoContractPathException(path)
    }
    path = lastContractPath;
  }

  lastContractPath = path;
  return { path, ext }
}

const packageName = 'package.json'

function findPackage(dirname: string): Maybe<string> {
  if (dirname === '') {
    return undefined;
  }
  if (existsSync(join(dirname, packageName))) {
    return dirname;
  }
  return findPackage(dirname.substring(0, dirname.lastIndexOf('/')));
}

export async function executeCommand(
  binary: BinaryInfo,
  command,
  client: LanguageClient,
  commandArgs = CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
  showOutput = true,
): Promise<string> {
  const contractInfo = getLastContractPath()
  const ligoPath = getBinaryPath(binary, vscode.workspace.getConfiguration());

  let finalCommand = command;
  if (commandArgs & CommandRequiredArguments.Path) {
    finalCommand = finalCommand(contractInfo.path)
  }
  if (commandArgs & CommandRequiredArguments.Ext) {
    finalCommand = finalCommand(extToDialect(contractInfo.ext))
  }
  if (commandArgs & CommandRequiredArguments.ProjectRoot) {
    finalCommand = finalCommand(findPackage(dirname(contractInfo.path)))
  }
  try {
    if (finalCommand.includes(undefined)) {
      throw new ex.UserInterruptionException()
    }

    const requestType = new RequestType<null, string | null, void>('indexDirectory')
    const indexDirectory: string | null = await client.sendRequest(requestType, null)
    const result = execFileSync(ligoPath, finalCommand, { cwd: indexDirectory }).toString()

    if (showOutput) {
      ligoOutput.appendLine(result)
      ligoOutput.show();
    }
    return result;
  } catch (error) {
    throw new ex.ExecutionException(error)
  }
}
