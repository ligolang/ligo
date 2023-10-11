import * as vscode from 'vscode'
import { LanguageClient } from 'vscode-languageclient/node'
import { TezosToolkit } from '@taquito/taquito';
import { importKey } from '@taquito/signer';

import fetch from 'node-fetch';
import { basename } from 'path';
import { createQuickPickBox } from '../ui';
import {
  CompileContractResult, CompileStorageResult, executeCompileContract, executeCompileStorage,
} from './ligoCommands';
import { getLastContractPath, ligoOutput } from './common';

const AUTHORIZATION_HEADER = 'Bearer ligo-ide';

const currentlyActiveNetworks = ['nairobinet', 'ghostnet']

const Tezos = (network: string): TezosToolkit | undefined => {
  if (currentlyActiveNetworks.includes(network)) {
    return new TezosToolkit(`https://${network}.ecadinfra.com`)
  } else {
    return undefined
  }
}

async function askForNetwork(): Promise<string> {
  return createQuickPickBox(
    currentlyActiveNetworks,
    'Network',
    'Choose a network to deploy contract to',
  );
}

type ContractAndStorage = {
  code: CompileContractResult,
  storage: CompileStorageResult
}

async function askForContractAndStorage(
  format: string,
  prevEntrypoint = undefined,
  prevStorage = undefined,
): Promise<ContractAndStorage> {
  const code = await executeCompileContract(
    prevEntrypoint,
    format,
    false,
  )

  const storage = await executeCompileStorage(
    code.entrypoint,
    format,
    prevStorage,
    false,
  )

  return { code, storage }
}

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

const showUnsupportedMessage = (network: string, msg?: string) => {
  const details = msg ? ` Details: ${msg}` : ''
  return vscode.window.showWarningMessage(
    `Currently, the extension does not support deployment on the ${network} testnet.${details}`,
  )
}

export async function executeDeploy(): Promise<void> {
  const { code, storage } = await askForContractAndStorage('json')
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

export async function executeGenerateDeployScript(): Promise<void> {
  const { code: codeJson, storage: storageJson } = await askForContractAndStorage('json')
  const { code, storage } = await askForContractAndStorage(
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
