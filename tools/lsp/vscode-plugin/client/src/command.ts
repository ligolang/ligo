import * as vscode from 'vscode'

import {
  LanguageClient,
} from 'vscode-languageclient/node'

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
}

export default LigoCommands

export function registerCommands(client: LanguageClient) {
  LigoCommands.StartServer.register(client)
  LigoCommands.StopServer.register(client)
  LigoCommands.RestartServer.register(client)
}
