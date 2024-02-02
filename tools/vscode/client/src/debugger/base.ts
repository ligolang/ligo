// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

import { execFileSync } from 'child_process';
import * as vscode from 'vscode';
import { isDefined, Maybe } from '../common/base';
import { ConfigCommand, ConfigField } from './LigoDebugConfigurationProvider';

// Interrupts debugging execution.
//
// Note if debugging session is started then you need
// to manually terminate it.
export function interruptExecution(): never {
  // 1000 - 7 is just a random value. We're throwing it
  // in order not to trigger all internal `vscode` executions.
  throw 1000 - 7;
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
  entrypoint?: string
  storage?: string
  moduleName?: string
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

export interface Entrypoints {
  [entrypoint: string]: string
}

export interface ContractMetadata {
  parameterMichelsonType: string
  storageMichelsonType: string
  entrypoints: Entrypoints
}

export type ContractMetadataFetcher = (file: string, logDir: string) => Promise<ContractMetadata>

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
  return vscode.workspace.workspaceFolders?.[0].uri;
}

export function generatedModuleName(module: string): string {
  if (module == "") {
    return "$main";
  } else {
    return module + ".$main";
  }
}
