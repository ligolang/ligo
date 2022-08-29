import * as vscode from 'vscode'
import { LanguageClient, RequestType } from 'vscode-languageclient/node'
import { extname, basename } from 'path';
import { execFileSync } from 'child_process';

import { extensions } from '../common'

import * as ex from '../exceptions'

export const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')

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
    case '.religo': return 'reasonligo'
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
export enum CommandRequiredArguments {
  NoArgs,
  Path,
  FileName,
  PathAndExt
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

export async function executeCommand(
  binary: BinaryInfo,
  command,
  client: LanguageClient,
  commandArgs = CommandRequiredArguments.Path,
  showOutput = true,
): Promise<string> {
  const contractInfo = getLastContractPath()
  const ligoPath = getBinaryPath(binary, vscode.workspace.getConfiguration());

  let finalCommand;
  switch (commandArgs) {
    case CommandRequiredArguments.NoArgs:
      finalCommand = command();
      break;
    case CommandRequiredArguments.Path:
      finalCommand = command(contractInfo.path);
      break;
    case CommandRequiredArguments.FileName:
      finalCommand = command(contractInfo.path, command(basename(contractInfo.path).split('.')[0]));
      break;
    case CommandRequiredArguments.PathAndExt:
      finalCommand = command(contractInfo.path, extToDialect(extname(contractInfo.path)));
      break;
    default:
      throw new ex.UnknownCommandTypeExtension()
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
