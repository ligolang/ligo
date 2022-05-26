import * as vscode from 'vscode'
import { LanguageClient } from 'vscode-languageclient/node'
import { TezosToolkit } from '@taquito/taquito';
import { importKey } from '@taquito/signer';

import fetch from 'node-fetch';
import { basename } from 'path';
import { createRememberingInputBox } from '../ui';
import { executeCompileContract, executeCompileStorage } from './ligoCommands';
import { getLastContractPath, ligoOutput } from './common';

const AUTHORIZATION_HEADER = 'Bearer ligo-ide';
const Tezos = (network: string) => {
  switch (network) {
    case 'mainnet':
      return new TezosToolkit('https://mainnet.api.tez.ie');
    case 'ithacanet':
      return new TezosToolkit('https://ithacanet.ecadinfra.com');
    default:
      vscode.window.showWarningMessage(`Currently extension does not support deployment on ${network} testnet`)
      return new TezosToolkit('empty');
  }
}

export async function fetchRandomPrivateKey(network: string): Promise<string> {
  const URL = `https://api.tez.ie/keys/${network}/`;
  const response = await fetch(URL, {
    method: 'POST',
    headers: { Authorization: AUTHORIZATION_HEADER },
  });
  return response.text();
}

export async function executeDeploy(client: LanguageClient) {
  const code = await executeCompileContract(
    client,
    undefined,
    'json',
    false,
    'Attempt to compile contract failed with exception:',
  )

  if (!code || !code.result) {
    return undefined;
  }

  const storage = await executeCompileStorage(
    client,
    code.entrypoint,
    'json',
    undefined,
    false,
    'Attempt to compile storage failed with exception:',
  )

  if (!storage || !storage.result) {
    return undefined;
  }

  const network = await createRememberingInputBox({
    title: 'Network',
    placeHolder: 'Choose a network to deploy contract to',
    rememberingKey: 'network',
    defaultValue: 'ithacanet',
  });

  if (!network) {
    return undefined;
  }

  vscode.window.withProgress(
    {
      cancellable: false,
      location: vscode.ProgressLocation.Notification,
      title: 'Deploying contract. It might take some time',
    },
    async (progress) => {
      try {
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

        return contract.address
      } catch (ex) {
        ligoOutput.appendLine(`Contract origination failed with exception: ${ex.message}`)
      } finally {
        ligoOutput.show()
      }

      return undefined;
    },
  )
  return undefined
}

export async function executeGenerateDeployScript(client: LanguageClient) {
  try {
    const codeJson = await executeCompileContract(
      client,
      undefined,
      'json',
      false,
      'Attempt to compile contract failed with exception:',
    )
    if (!codeJson || !codeJson.result) {
      return undefined;
    }

    const storageJson = await executeCompileStorage(
      client,
      codeJson.entrypoint,
      'json',
      undefined,
      false,
      'Attempt to compile storage failed with exception:',
    )

    if (!storageJson || !storageJson.result) {
      return undefined;
    }

    const code = await executeCompileContract(client, codeJson.entrypoint, 'text')
    if (!code) {
      return undefined;
    }

    const storage = await executeCompileStorage(client, storageJson.entrypoint, 'text', storageJson.storage)
    if (!storage) {
      return undefined;
    }

    const network = await createRememberingInputBox({
      title: 'Network',
      placeHolder: 'Choose a network to deploy contract to',
      rememberingKey: 'network',
      defaultValue: 'ithacanet',
    });

    if (!network) {
      return undefined;
    }
    const contractInfo = getLastContractPath()
    const name = basename(contractInfo.path).split('.')[0]

    vscode.window.withProgress(
      {
        cancellable: false,
        location: vscode.ProgressLocation.Notification,
        title: 'Generating deploy contract. It might take some time',
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
          'tezos-client',
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
          estimate.burnFeeMutez / 1000000,
        ]

        ligoOutput.appendLine(res.join(' '))
        ligoOutput.show()
      },
    )
  } catch (ex) {
    ligoOutput.appendLine(`Contract origination failed with exception: ${ex.message}`)
    ligoOutput.show()
  }

  return undefined
}
