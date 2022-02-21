import * as vscode from 'vscode'
import { exec } from 'child_process'

import {
  LanguageClient,
} from 'vscode-languageclient/node'

import getLigoPath from './updateLigo'

const ligoOutput = vscode.window.createOutputChannel('LIGO compiler')

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
      const path = vscode.window.activeTextEditor.document.uri.fsPath;
      const ligoPath = getLigoPath();
      exec(`${ligoPath} compile contract ${path}`, (error, stdout, stderr) => {
        if (error) {
          ligoOutput.appendLine(`error: ${error.message}`);
        } else if (stderr) {
          ligoOutput.appendLine(`stderr: ${stderr}`)
        } else {
          ligoOutput.appendLine(stdout)
        }
      });
      ligoOutput.show();
    },
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileCode.name,
      async () => LigoCommands.CompileCode.run(),
    )
  },
}

export default LigoCommands

export function registerCommands(client: LanguageClient) {
  LigoCommands.StartServer.register(client)
  LigoCommands.StopServer.register(client)
  LigoCommands.RestartServer.register(client)
  LigoCommands.CompileCode.register()
}
