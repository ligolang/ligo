import * as vscode from 'vscode'
import { TezosToolkit } from '@taquito/taquito';
import { importKey } from '@taquito/signer';

import fetch from 'node-fetch';
import { basename } from 'path';
import { createQuickPickBox } from '../ui';
import {
  CompileContractResult, CompileStorageResult, executeCompileContract, executeCompileStorage,
} from './ligoCommands';
import { getLastContractPath, ligoOutput } from './common';
import { LigoContext } from '../../common/LigoContext';
import { LigoProtocolClient } from '../../common/LigoProtocolClient';
import { Maybe } from '../../common/base';

/** Used by {@link fetchRandomPrivateKey}. */
const AUTHORIZATION_HEADER = 'Bearer ligo-ide';

/** List of active supported networks for deploying Michelson contracts. */
const currentlyActiveNetworks = ['ghostnet']

/**
 * Tezos toolkit to be used for deploying contracts. Currently deploys them to
 * ecadinfra.
 */
const Tezos = (network: string): TezosToolkit | undefined => {
  if (currentlyActiveNetworks.includes(network)) {
    return new TezosToolkit(`https://${network}.ecadinfra.com`)
  } else {
    return undefined
  }
}

/**
 * Opens a quick pick asking the user to choose a network to deploy contracts,
 * using the networks from {@link currentlyActiveNetworks}.
 *
 * @returns The user-inputted network for deploying a contract.
 */
async function askForNetwork(): Promise<string> {
  return createQuickPickBox(
    currentlyActiveNetworks,
    'Network',
    'Choose a network to deploy contract to',
  );
}

/** Utility data type holding a compiled contract and compiled storage. */
type ContractAndStorage = {
  /** Result of compiling the contract. */
  code: CompileContractResult,

  /** Result of compiling the storage. */
  storage: CompileStorageResult
}

/**
 * Opens input boxes asking the user to provide contract and storage data for
 * compilation.
 *
 * @param format The format (text, hex, json) for the output chosen by the user.
 * @param prevEntrypoint The entry-point to which we are getting the storage.
 * Asks the user for input if not provided.
 * @param prevStorage The storage to be compiled. Asks the user for input if not
 * provided.
 * @returns The result of compiling the contract and storage.
 */
async function askForContractAndStorage(
  context: LigoContext,
  client: LigoProtocolClient,
  format: string,
  prevEntrypoint: Maybe<string> = undefined,
  prevStorage: Maybe<string> = undefined,
): Promise<ContractAndStorage> {
  const code = await executeCompileContract(
    context,
    client,
    prevEntrypoint,
    format,
    false,
  )

  const storage = await executeCompileStorage(
    context,
    client,
    code.entrypoint,
    format,
    prevStorage,
    false,
  )

  return { code, storage }
}

/**
 * Fetches a random private key from ecadinfra used for deploying a smart
 * contract to the blockchain.
 *
 * @param network The network to which the contract will be deployed.
 * @returns A promise resolving to the key, or an error message, depending on
 * whether we got "OK" or "Error".
 */
export async function fetchRandomPrivateKey(network: string): Promise<["OK" | "Error", string]> {
  const URL = `https://keygen.ecadinfra.com/${network}`;
  const response = await fetch(URL, {
    method: 'POST',
    headers: { Authorization: AUTHORIZATION_HEADER },
  });

  const status = response.status;
  const text = await response.text()
  return [status === 200 ? "OK" : "Error", text]
}

/**
 * Displays a warning window to the user explaining that we do not support
 * deploying to the given network.
 *
 * @param network The unsupported network.
 * @param msg An optional message providing more details.
 */
const showUnsupportedMessage = (network: string, msg?: string) => {
  const details = msg ? ` Details: ${msg}` : ''
  return vscode.window.showWarningMessage(
    `Currently, the extension does not support deployment on the ${network} testnet.${details}`,
  )
}

/**
 * Asks the user for input and deploys the smart contract to the network that
 * will be chosen by the user. The output will be printed to the LIGO Compiler
 * output channel.
 */
export async function executeDeploy(context: LigoContext, client: LigoProtocolClient): Promise<void> {
  const { code, storage } = await askForContractAndStorage(context, client, 'json')
  const network = await askForNetwork()

  vscode.window.withProgress(
    {
      cancellable: false,
      location: vscode.ProgressLocation.Notification,
      title: 'Deploying contract. It might take some time',
    },
    async (progress) => {
      const TezosNetwork = Tezos(network)
      if (!TezosNetwork) {
        showUnsupportedMessage(network)
        return
      }

      progress.report({
        message: 'Generating key',
      })

      const [status, result] = await fetchRandomPrivateKey(network)
      if (status === "Error") {
        showUnsupportedMessage(network, result)
        return
      }

      await importKey(TezosNetwork, result);

      progress.report({
        message: 'Originating contract',
      })

      const op = await TezosNetwork.contract.originate({
        code: JSON.parse(code.result),
        init: JSON.parse(storage.result),
      });

      // TODO: Sometimes 'better-call.dev' link doesn't lead to the contract info.
      // (see: https://gitlab.com/serokell/ligo/ligo/-/merge_requests/282#note_921119098)
      // We need to examine why this happens and to fix it if possible
      const contract = await op.contract();
      ligoOutput.appendLine(`The contract was successfully deployed on the ${network} test network`)
      ligoOutput.appendLine(`View your contract here: https://better-call.dev/${network}/${contract.address}`)
      ligoOutput.appendLine(`The address of your new contract is: ${contract.address}`)
      ligoOutput.appendLine(`The initial storage of your contract is: ${storage.result}`)
      ligoOutput.show()

      return contract.address
    },
  )
}

/**
 * Asks the user for input and generates a script that can be used to call
 * `octez-client` to deploy the smart contract to the network that will be
 * chosen by the user. The output will be printed to the LIGO Compiler output
 * channel.
 */
export async function executeGenerateDeployScript(context: LigoContext, client: LigoProtocolClient): Promise<void> {
  const { code: codeJson, storage: storageJson } = await askForContractAndStorage(context, client, 'json')
  const { code, storage } = await askForContractAndStorage(
    context,
    client,
    'text',
    codeJson.entrypoint,
    storageJson.storage,
  )

  const network = await askForNetwork()

  const contractInfo = getLastContractPath()
  const name = basename(contractInfo.path).split('.')[0]

  vscode.window.withProgress(
    {
      cancellable: false,
      location: vscode.ProgressLocation.Notification,
      title: 'Generating deploy script. It might take some time',
    },
    async (progress) => {
      const TezosNetwork = Tezos(network)
      if (!TezosNetwork) {
        showUnsupportedMessage(network)
        return
      }

      progress.report({
        message: 'Generating key',
      })

      const [status, result] = await fetchRandomPrivateKey(network)
      if (status === "Error") {
        showUnsupportedMessage(network, result)
        return
      }

      const randomPrivateKey = result
      await importKey(TezosNetwork, randomPrivateKey);
      progress.report({
        message: 'Calculating burn-cap cost',
      })

      const estimate = await TezosNetwork.estimate.originate({
        code: JSON.parse(codeJson.result),
        init: JSON.parse(storageJson.result),
      });

      const sourceAccount = vscode.workspace.getConfiguration().get<string>('ligoLanguageServer.tezos_source_account');

      ligoOutput.appendLine(`Generated deploy script for '${name}' contract:`);
      const res = [
        'octez-client',
        'originate',
        'contract',
        name,
        'transferring',
        '0',
        'from',
        sourceAccount,
        'running',
        code.result,
        '--init',
        storage.result,
        '--burn-cap',
        estimate.burnFeeMutez / 1_000_000,
      ]

      ligoOutput.appendLine(res.join(' '))
      ligoOutput.show()
    },
  )
}
