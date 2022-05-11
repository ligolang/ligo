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
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileContract.name,
      async () => LigoCommands.CompileContract.run(),
    ),
  },
  CompileStorage: {
    name: 'ligo.compileStorage',
    run: () => lc.executeCompileStorage(),
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileStorage.name,
      async () => LigoCommands.CompileStorage.run(),
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
  Deploy: {
    name: 'ligo.deploy',
    run: tc.executeDeploy,
    register: () => vscode.commands.registerCommand(
      LigoCommands.Deploy.name,
      async () => LigoCommands.Deploy.run(),
    ),
  },
  GenerateDeployScript: {
    name: 'ligo.generateDeployScript',
    run: tc.executeGenerateDeployScript,
    register: () => vscode.commands.registerCommand(
      LigoCommands.GenerateDeployScript.name,
      async () => LigoCommands.GenerateDeployScript.run(),
    ),
  },
  ChooseLigoOption: {
    name: 'ligo.chooseOption',
    run: async () => {
      const possibleOptions = ['Compile contract', 'Compile storage', 'Compile expression', 'Dry run', 'Evaluate function', 'Evaluate value']
      const curCommand = await ui.createQuickPickBox(possibleOptions, 'ligo compiler command', 'command')
      switch (curCommand) {
        case 'Compile contract':
          lc.executeCompileContract();
          break;
        case 'Compile storage':
          lc.executeCompileStorage();
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
  ChooseDeploymentOption: {
    name: 'tezos.chooseOption',
    run: async () => {
      const possibleOptions = ['Deploy contract', 'Generate deploy script']
      const curCommand = await ui.createQuickPickBox(possibleOptions, 'Deployment options', 'command')
      switch (curCommand) {
        case 'Deploy contract':
          tc.executeDeploy();
          break;
        case 'Generate deploy script':
          tc.executeGenerateDeployScript();
          break;
        default:
          console.error('Unknown command');
      }
    },
    register: () => vscode.commands.registerCommand(
      LigoCommands.ChooseDeploymentOption.name,
      async () => LigoCommands.ChooseDeploymentOption.run(),
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
  LigoCommands.ChooseLigoOption.register();
  LigoCommands.ChooseDeploymentOption.register();
  // LIGO commands
  LigoCommands.CompileContract.register();
  LigoCommands.CompileExpression.register();
  LigoCommands.CompileStorage.register();
  LigoCommands.DryRun.register();
  LigoCommands.EvaluateFunction.register();
  LigoCommands.EvaluateValue.register();
  // Tezos commands
  LigoCommands.Deploy.register();
  LigoCommands.GenerateDeployScript.register();
}
