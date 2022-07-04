import * as vscode from 'vscode'

import {
  LanguageClient,
} from 'vscode-languageclient/node'

import * as lc from './commands/ligoCommands'
import * as tc from './commands/tezosCommands'
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
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.CompileContract.name,
      async () => LigoCommands.CompileContract.run(client),
    ),
  },
  SilentCompileContract: {
    name: 'ligo.silentCompileContract',
    run: lc.executeSilentCompileContract,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.SilentCompileContract.name,
      async (
        options: lc.SilentCompilationOptions,
      ) => LigoCommands.SilentCompileContract.run(client, options),
    ),
  },
  CompileStorage: {
    name: 'ligo.compileStorage',
    run: lc.executeCompileStorage,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.CompileStorage.name,
      async () => LigoCommands.CompileStorage.run(client),
    ),
  },
  CompileExpression: {
    name: 'ligo.compileExpression',
    run: lc.executeCompileExpression,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.CompileExpression.name,
      async () => LigoCommands.CompileExpression.run(client),
    ),
  },
  DryRun: {
    name: 'ligo.dryRun',
    run: lc.executeDryRun,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.DryRun.name,
      async () => LigoCommands.DryRun.run(client),
    ),
  },
  EvaluateFunction: {
    name: 'ligo.evaluateFunction',
    run: lc.executeEvaluateFunction,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.EvaluateFunction.name,
      async () => LigoCommands.EvaluateFunction.run(client),
    ),
  },
  EvaluateValue: {
    name: 'ligo.evaluateValue',
    run: lc.executeEvaluateValue,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.EvaluateValue.name,
      async () => LigoCommands.EvaluateValue.run(client),
    ),
  },
  Deploy: {
    name: 'ligo.deploy',
    run: tc.executeDeploy,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.Deploy.name,
      async () => LigoCommands.Deploy.run(client),
    ),
  },
  GenerateDeployScript: {
    name: 'ligo.generateDeployScript',
    run: tc.executeGenerateDeployScript,
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.GenerateDeployScript.name,
      async () => LigoCommands.GenerateDeployScript.run(client),
    ),
  },
  ChooseLigoOption: {
    name: 'ligo.chooseOption',
    run: async (client: LanguageClient) => {
      const possibleOptions = ['Compile contract', 'Compile storage', 'Compile expression', 'Dry run', 'Evaluate function', 'Evaluate value']
      const curCommand = await ui.createQuickPickBox(possibleOptions, 'ligo compiler command', 'command')
      switch (curCommand) {
        case 'Compile contract':
          lc.executeCompileContract(client);
          break;
        case 'Compile storage':
          lc.executeCompileStorage(client);
          break;
        case 'Compile expression':
          lc.executeCompileExpression(client);
          break;
        case 'Dry run':
          lc.executeDryRun(client);
          break;
        case 'Evaluate function':
          lc.executeEvaluateFunction(client);
          break;
        case 'Evaluate value':
          lc.executeEvaluateValue(client);
          break;
        default:
          console.error('Unknown command');
      }
    },
    register: (client: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.ChooseLigoOption.name,
      async () => LigoCommands.ChooseLigoOption.run(client),
    ),
  },
  ChooseDeploymentOption: {
    name: 'tezos.chooseOption',
    run: async (client: LanguageClient) => {
      const possibleOptions = ['Deploy contract', 'Generate deploy script']
      const curCommand = await ui.createQuickPickBox(possibleOptions, 'Deployment options', 'command')
      switch (curCommand) {
        case 'Deploy contract':
          tc.executeDeploy(client);
          break;
        case 'Generate deploy script':
          tc.executeGenerateDeployScript(client);
          break;
        default:
          console.error('Unknown command');
      }
    },
    register: (client:LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.ChooseDeploymentOption.name,
      async () => LigoCommands.ChooseDeploymentOption.run(client),
    ),
  },
}

export default LigoCommands

export function registerCommands(client: LanguageClient) {
  // Boot
  LigoCommands.StartServer.register(client);
  LigoCommands.StopServer.register(client);
  LigoCommands.RestartServer.register(client);
  // Buttons
  LigoCommands.ChooseLigoOption.register(client);
  LigoCommands.ChooseDeploymentOption.register(client);
  // LIGO commands
  LigoCommands.CompileContract.register(client);
  LigoCommands.CompileExpression.register(client);
  LigoCommands.CompileStorage.register(client);
  LigoCommands.DryRun.register(client);
  LigoCommands.EvaluateFunction.register(client);
  LigoCommands.EvaluateValue.register(client);
  // Tezos commands
  LigoCommands.Deploy.register(client);
  LigoCommands.GenerateDeployScript.register(client);
  // Additional commands
  LigoCommands.SilentCompileContract.register(client);
}
