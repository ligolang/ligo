import * as os from 'os'
import * as vscode from 'vscode'
import { LanguageClient, RequestType } from 'vscode-languageclient/node'
import { extname, join, dirname } from 'path';
import { existsSync } from 'fs'
import { execFileSync } from 'child_process';

import { extensions, Maybe } from '../common'

import * as ex from '../exceptions'

export const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')

/* eslint-disable no-bitwise */

let lastContractPath: string;

type BinaryInfo = {
  name: string,
  path: string,
}

export const ligoBinaryInfo = {
  name: 'ligo',
  path: 'ligoLanguageServer.ligoBinaryPath'
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

export function findBinaryPath(binaryName: string): string {
  const find = os.platform() === 'win32' ? 'where.exe' : 'which'
  return execFileSync(find, [binaryName]).toString().trim().split('\n')[0]
}

export function getBinaryPath(info: BinaryInfo, config: vscode.WorkspaceConfiguration) {
  let binaryPath = config.get<string>(info.path)
  if (binaryPath) {
    return binaryPath
  }

  try {
    vscode.window.showWarningMessage(`'${info.name}' binary not found through the LIGO configuration. Searching in PATH.`)
    binaryPath = findBinaryPath(info.name)
    config.update(info.path, binaryPath)
    vscode.window.showWarningMessage(`'${info.path}' variable was updated to ${binaryPath}`)
    return binaryPath
  } catch {
    throw new ex.BinaryNotFoundException(info.name)
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

export async function executeCommand(
  binary: BinaryInfo,
  command: any,
  client: LanguageClient,
  commandArgs = CommandRequiredArguments.Path | CommandRequiredArguments.ProjectRoot,
  showOutput = true,
): Promise<string> {
  const contractInfo = getLastContractPath()
  const ligoPath = getBinaryPath(binary, vscode.workspace.getConfiguration());

  if (commandArgs & CommandRequiredArguments.Path) {

    command = command(contractInfo.path)
  }
  if (commandArgs & CommandRequiredArguments.Ext) {
    command = command(extToDialect(contractInfo.ext))
  }
  if (commandArgs & CommandRequiredArguments.ProjectRoot) {
    command = command(findPackage(dirname(contractInfo.path)))
  }
  try {
    if (command.includes(undefined)) {
      throw new ex.UserInterruptionException()
    }

    // FIXME (#1657): Use the code below when we support getting the index
    // directory and use the propery `indexDirectory` for the `cwd`.
    /*
    const requestType = new RequestType<null, string | null, void>('indexDirectory')
    const indexDirectory: string | null = await client.sendRequest(requestType, null)
    */
    const indexDirectory: string | null = null
    const result = execFileSync(ligoPath, command, { cwd: indexDirectory }).toString()

    if (showOutput) {
      ligoOutput.appendLine(result)
      ligoOutput.show();
    }
    return result;
  } catch (error) {
    throw new ex.ExecutionException(error)
  }
}
