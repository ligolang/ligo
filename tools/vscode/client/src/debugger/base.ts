// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

import * as vscode from 'vscode';
import { isDefined, Maybe } from '../common/base';
import { ConfigCommand, ConfigField } from './LigoDebugConfigurationProvider';

/**
 * Interrupts the debugging execution.
 *
 * Note if debugging session is started then you need to manually terminate it.
 */
export function interruptExecution(): never {
  // 1000 - 7 is just a random value. We're throwing it
  // in order not to trigger all internal `vscode` executions.
  throw 1000 - 7;
}

/** Extracts the name of a pseudo-command (like `(*@AskOnStart@*)`). */
export function getCommand(str: Maybe<string>): Maybe<string> {
  if (isDefined(str)) {
    const matches = str.match(/^\(\*\@(.*)\@\*\)$/);
    if (isDefined(matches)) {
      return matches[1];
    }
  }
}

/** Parameters for launching LIGO Debugger. */
export interface LigoLaunchRequest {
  /** Launches the contract without enabling debugging. */
  noDebug?: boolean

  /** The contract path to debug. */
  program?: string

  /** The entry point name to debug. */
  entrypoint?: string

  /** The storage provided to the entry point. */
  storage?: string

  /**
   * The module name to be debugged. This will be asked if there are two or
   * more modules.
   */
  moduleName?: string

  /** The parameter provided to the entry point. */
  parameter?: string

  /** Tezos environment that will be provided to the contract. */
  contractEnv?: LigoContractEnv
}

/** Tezos environment used while stepping through a contract. */
export interface LigoContractEnv {
  /** Current time. */
  now?: string

  /** Current balance. */
  balance?: number

  /** Amount of the current transaction. */
  amount?: number

  /** Address of the current contract. */
  self?: string

  /** Address of the contract that initiated the current execution. */
  source?: string

  /** Address of the contract that initiated the current internal execution. */
  sender?: string

  /** The identifier of the chain on which this contract is executed. */
  chainId?: string

  /** The current block level. */
  level?: number

  /** The voting power for the current contract. */
  votingPowers?: VotingPowers
}

/** Data type representing Tezos voting power. */
export interface VotingPowers {
  kind: "simple"
  contents: Map<string, number>
}

/** Data type representing a map from the entry point to its Michelson type. */
export interface Entrypoints {
  [entrypoint: string]: string
}

/** Data type representing metadata (parameter, storage, entry points). */
export interface ContractMetadata {
  parameterMichelsonType: string
  storageMichelsonType: string
  entrypoints: Entrypoints
}

/**
 * A callback to fetch a contract's metadata.
 *
 * @param file The file path to debug.
 * @param logDir The directory to create a log file.
 */
export type ContractMetadataFetcher = (file: string, logDir: string) => Promise<ContractMetadata>

/**
 * Attempts to execute a command based on the provided parameters and
 * conditions. Returns a default value in case the provided configuration item
 * doesn't have a matching pseudo-command.
 *
 * @param field The configuration field for which the command is being executed.
 * @param expectedExtractedCommand The expected command extracted from the
 * configuration item. Throws an exception if it doesn't match the actual
 * extracted command.
 * @param configItem The configuration item.
 * @param resultPromise A function that returns a Promise resolving to the
 * result of executing the command.
 * @param defaultItem The default value for the configuration item if no result
 * is obtained.
 * @param shouldInterruptOnClose A flag indicating whether to interrupt
 * execution if the user closes the entry point quick pick.
 * @returns A Promise resolving to the result of executing the command, or the
 * default item if no result is obtained.
 */
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
      throw new Error(`Expected command for field "${field}" is "${expectedExtractedCommand}". Got "${extractedCommand}".`);
    }
  } else {
    return defaultItem;
  }
}

/**
 * Values of type never should be uninhabited. This function is used to throw an
 * error in branches where this value was obtained. This is roughly the same as
 * the `absurd` function in Haskell.
 */
export function impossible(x: never) {
  throw new Error(`An "impossible" value happened! Value: ${x}`)
}

/** Returns the current VSCode workspace file path. */
export function getCurrentWorkspacePath(): Maybe<vscode.Uri> {
  return vscode.workspace.workspaceFolders?.[0].uri;
}

/**
 * Returns the LIGO-generated name of a module (i.e., `$main`). If a module is
 * provided, it will return `${module}.$main`.
 */
export function generatedModuleName(module: string): string {
  if (module == "") {
    return "$main";
  } else {
    return module + ".$main";
  }
}
