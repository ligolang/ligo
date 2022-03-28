import * as vscode from 'vscode'
import { execFile } from 'child_process'
import { extname } from 'path';

import {
  LanguageClient,
} from 'vscode-languageclient/node'

import { getLigoPath } from './updateLigo'

import createRememberingInputBox from './ui'

const ligoOutput = vscode.window.createOutputChannel('LIGO compiler')
let lastContractPath;

const LigoCommands = {
  StartServer: {
    name: 'ligo.startServer',
    run: async (client: LanguageClient) => {
      client.info('Starting LIGO LSP server')
      await client.start()
      client.info('Started LIGO LSP server')
    },
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.StartServer.name,
      async () => LigoCommands.StartServer.run(client),
    ),
  },
  StopServer: {
    name: 'ligo.stopServer',
    run: async (client: LanguageClient) => {
      client.info('Stopping LIGO LSP server')
      await client.stop()
      client.info('Stopped LIGO LSP server')
    },
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.StopServer.name,
      async () => LigoCommands.StopServer.run(client),
    ),
  },
  RestartServer: {
    name: 'ligo.restartServer',
    run: async (client: LanguageClient) => {
      await LigoCommands.StopServer.run(client)
      await LigoCommands.StartServer.run(client)
    },
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.RestartServer.name,
      async () => LigoCommands.RestartServer.run(client),
    ),
  },
  CompileCode: {
    name: 'ligo.compileContract',
    run: async () => {
      const maybeEntrypoint = await createRememberingInputBox('Entrypoint', 'Enter entrypoint to compile', 'main');
      if (!maybeEntrypoint) {
        return;
      }

      let path = vscode.window.activeTextEditor.document.uri.fsPath;
      const ext = extname(path);

      if (ext !== '.ligo' && ext !== '.mligo' && ext !== '.religo') {
        if (!lastContractPath) {
          return;
        }
        path = lastContractPath;
      }

      const ligoPath = getLigoPath(vscode.workspace.getConfiguration());
      lastContractPath = path;

      if (ligoPath === undefined) {
        return;
      }
      execFile(ligoPath, ['compile', 'contract', path, '-e', maybeEntrypoint], (error, stdout, stderr) => {
        if (error) {
          ligoOutput.appendLine(error.message);
        } else {
          ligoOutput.appendLine(stderr);
          ligoOutput.appendLine(stdout);
        }
      });
      ligoOutput.show();
    },
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileCode.name,
      async () => LigoCommands.CompileCode.run(),
    ),
  },
}

export default LigoCommands

export function registerCommands(client: LanguageClient) {
  LigoCommands.StartServer.register(client)
  LigoCommands.StopServer.register(client)
  LigoCommands.RestartServer.register(client)
  LigoCommands.CompileCode.register()
}
