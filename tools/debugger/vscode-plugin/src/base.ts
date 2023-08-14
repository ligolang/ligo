// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

import { execFileSync } from 'child_process';
import * as vscode from 'vscode';
import { ConfigCommand, ConfigField } from './LigoDebugConfigurationProvider';

export type Maybe<T> = T | undefined

// Interrupts debugging execution.
//
// Note if debugging session is started then you need
// to manually terminate it.
export function interruptExecution(): never {
  // 1000 - 7 is just a random value. We're throwing it
  // in order not to trigger all internal `vscode` executions.
  throw 1000 - 7;
}

// Make from an object reference
export type Ref<T> = {
  ref: T
}

export function isDefined<T>(x: T | undefined | null): x is T {
  return x !== null && x !== undefined
}

// Extract the name of a pseudo-command (like `(*@AskOnStart@*)`).
export function getCommand(str: Maybe<string>): Maybe<string> {
  if (isDefined(str)) {
    const matches = str.match(/^\(\*\@(.*)\@\*\)$/);
    if (isDefined(matches)) {
      return matches[1];
    }
  }
}

export interface LigoLaunchRequest {
  noDebug?: boolean
  program?: string
  michelsonEntrypoint?: string
  storage?: string
  entrypoint?: string
  parameter?: string
  contractEnv?: LigoContractEnv
}

export interface LigoContractEnv {
  now?: string
  balance?: number
  amount?: number
  self?: string
  source?: string
  sender?: string
  chainId?: string
  level?: number
  votingPowers?: VotingPowers
}

export interface VotingPowers {
  kind: "simple"
  contents: Map<string, number>
}

export interface MichelsonEntrypoints {
  [entrypoint: string]: string
}

export interface ContractMetadata {
  parameterMichelsonType: string
  storageMichelsonType: string
  michelsonEntrypoints: MichelsonEntrypoints
}

export type ContractMetadataFetcher = (file: string, logDir: string) => Promise<ContractMetadata>

// Type if input box that initializ
export type InputBoxType = "parameter" | "storage"

export type InputValueLang = "LIGO" | "Michelson";

/** The type of validation in an input box.
 *
 * `undefined` would stand for validation pass, and a `string` would mean
 * the reason of a failure.
 */
export type InputValidationResult = Maybe<string>

type BinaryInfo = {
  name: string,
  path: string,
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
    vscode.window.showWarningMessage(`'${info.name}' binary not found in PATH. You won't be able to compile and deploy contracts
                                      without providing path to ${info.name} using ${info.path} variable,
                                      located in VSCode settings`)

    return undefined
  }
}

export async function tryExecuteCommand<T extends Maybe<string>>(
  field: ConfigField,
  expectedExtractedCommand: ConfigCommand,
  configItem: T,
  resultPromise: () => Promise<Maybe<T>>,
  defaultItem: T = configItem,
  shouldInterruptOnClose = true,
): Promise<Maybe<T>> {
  const extractedCommand = getCommand(configItem);
  if (isDefined(extractedCommand)) {
    if (extractedCommand === expectedExtractedCommand) {
      const result: Maybe<T> = await resultPromise();
      if (!isDefined(result) && shouldInterruptOnClose) {
        // If user decided to close entrypoint quickpick
        // then we want to stop debugging session immediately.
        // We can do this by throwing something (tried to throw `Error`
        // but I see annoying error message in bottom right corner).
        interruptExecution();
      } else {
        return result;
      }
    } else {
      throw new Error("Expected command for field \"" + field + "\" is \"" + expectedExtractedCommand + "\". Got \"" + extractedCommand + "\"");
    }
  } else {
    return defaultItem;
  }
}

export function impossible(x: never) {
  throw new Error("An impossible happened! Value: " + x)
}

export function getCurrentWorkspacePath(): Maybe<vscode.Uri> {
  return vscode.workspace.workspaceFolders?.[0].uri;;
}
