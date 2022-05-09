import * as vscode from 'vscode'

import {
  LanguageClient,
} from 'vscode-languageclient/node'

import * as lc from './ligoCommands'
import * as ui from './ui'

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
  CompileContract: {
    name: 'ligo.compileContract',
    run: lc.executeCompileContract,
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileContract.name,
      async () => LigoCommands.CompileContract.run(),
    ),
  },
  CompileExpression: {
    name: 'ligo.compileExpression',
    run: lc.executeCompileExpression,
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileExpression.name,
      async () => LigoCommands.CompileExpression.run(),
    ),
  },
  DryRun: {
    name: 'ligo.dryRun',
    run: lc.executeDryRun,
    register: () => vscode.commands.registerCommand(
      LigoCommands.DryRun.name,
      async () => LigoCommands.DryRun.run(),
    ),
  },
  EvaluateFunction: {
    name: 'ligo.evaluateFunction',
    run: lc.executeEvaluateFunction,
    register: () => vscode.commands.registerCommand(
      LigoCommands.EvaluateFunction.name,
      async () => LigoCommands.EvaluateFunction.run(),
    ),
  },
  EvaluateValue: {
    name: 'ligo.evaluateValue',
    run: lc.executeEvaluateValue,
    register: () => vscode.commands.registerCommand(
      LigoCommands.EvaluateValue.name,
      async () => LigoCommands.EvaluateValue.run(),
    ),
  },
  ChooseLigoOption: {
    name: 'ligo.chooseOption',
    run: async () => {
      const possibleOptions = ['Compile contract', 'Compile expression', 'Dry run', 'Evaluate function', 'Evaluate value']
      const curCommand = await ui.createQuickPickBox(possibleOptions, 'ligo compiler command', 'command')
      switch (curCommand) {
        case 'Compile contract':
          lc.executeCompileContract();
          break;
        case 'Compile expression':
          lc.executeCompileExpression();
          break;
        case 'Dry run':
          lc.executeDryRun();
          break;
        case 'Evaluate function':
          lc.executeEvaluateFunction();
          break;
        case 'Evaluate value':
          lc.executeEvaluateValue();
          break;
        default:
          console.error('Unknown command');
      }
    },
    register: () => vscode.commands.registerCommand(
      LigoCommands.ChooseLigoOption.name,
      async () => LigoCommands.ChooseLigoOption.run(),
    ),
  },
}

export default LigoCommands

export function registerCommands(client: LanguageClient) {
  LigoCommands.StartServer.register(client);
  LigoCommands.StopServer.register(client);
  LigoCommands.RestartServer.register(client);
  LigoCommands.ChooseLigoOption.register();
  LigoCommands.CompileContract.register();
  LigoCommands.CompileExpression.register();
  LigoCommands.DryRun.register();
  LigoCommands.EvaluateFunction.register();
  LigoCommands.EvaluateValue.register();
}
