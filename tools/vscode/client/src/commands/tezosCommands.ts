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
const currentlyActiveProtocol = 'mumbainet'
const Tezos = (network: string) => {
  switch (network) {
    case 'mainnet':
      return new TezosToolkit('https://mainnet.api.tez.ie');
    case currentlyActiveProtocol:
      return new TezosToolkit(`https://${currentlyActiveProtocol}.ecadinfra.com`);
    default:
      vscode.window.showWarningMessage(`Currently extension does not support deployment on ${network} testnet`)
      return new TezosToolkit('empty');
  }
}

async function askForNetwork(): Promise<string> {
  return createQuickPickBox(
    [currentlyActiveProtocol],
    'Network',
    'Choose a network to deploy contract to',
  );
}

type ContractAndStorage = {
  code: CompileContractResult,
  storage: CompileStorageResult
}

async function askForContractAndStorage(
  client: LanguageClient,
  format: string,
  prevEntrypoint = undefined,
  prevStorage = undefined,
): Promise<ContractAndStorage> {
  const code = await executeCompileContract(
    client,
    prevEntrypoint,
    format,
    false,
  )

  const storage = await executeCompileStorage(
    client,
    code.entrypoint,
    format,
    prevStorage,
    false,
  )

  return { code, storage }
}

export async function fetchRandomPrivateKey(network: string): Promise<string> {
  const URL = `https://api.tez.ie/keys/${network}/`;
  const response = await fetch(URL, {
    method: 'POST',
    headers: { Authorization: AUTHORIZATION_HEADER },
  });
  return response.text();
}

export async function executeDeploy(client: LanguageClient): Promise<void> {
  const { code, storage } = await askForContractAndStorage(client, 'json')
  const network = await askForNetwork()

  vscode.window.withProgress(
    {
      cancellable: false,
      location: vscode.ProgressLocation.Notification,
      title: 'Deploying contract. It might take some time\n',
    },
    async (progress) => {
      const TezosNetwork = Tezos(network)

      progress.report({
        message: 'Generating key',
      })

      await importKey(TezosNetwork, await fetchRandomPrivateKey(network));

      progress.report({
        message: 'Key generated, originating contract',
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

export async function executeGenerateDeployScript(client: LanguageClient): Promise<void> {
  const { code: codeJson, storage: storageJson } = await askForContractAndStorage(client, 'json')
  const { code, storage } = await askForContractAndStorage(
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
      progress.report({
        message: 'Generating key',
      })

      await importKey(TezosNetwork, await fetchRandomPrivateKey(network));
      progress.report({
        message: 'Key generated, calculating burn-cap cost',
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
