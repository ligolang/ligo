import * as vscode from 'vscode'

import {
  LanguageClient,
} from 'vscode-languageclient/node'
import { ligoOutput } from './commands/common'

import * as lc from './commands/ligoCommands'
import * as tc from './commands/tezosCommands'
import * as ui from './ui'
import * as ex from './exceptions'

function reportException(exception: Error) {
  if (!(exception instanceof ex.UserInterruptionException)) {
    vscode.window.showErrorMessage('Exception happened during execution.\n'
                                   + 'See console (LIGO Compiler) for additional information', 'Ok')
    ligoOutput.appendLine(exception.message)
    ligoOutput.show()
  }
}

async function executeChooseLigoOption() {
  const possibleOptions = ['Compile contract', 'Compile storage', 'Compile expression', 'Dry run', 'Evaluate function', 'Evaluate value']
  const curCommand = await ui.createQuickPickBox(possibleOptions, 'ligo compiler command', 'command')
  switch (curCommand) {
    case 'Compile contract':
      await lc.executeCompileContract()
      break;
    case 'Compile storage':
      await lc.executeCompileStorage()
      break;
    case 'Compile expression':
      await lc.executeCompileExpression()
      break;
    case 'Dry run':
      await lc.executeDryRun()
      break;
    case 'Evaluate function':
      await lc.executeEvaluateFunction()
      break;
    case 'Evaluate value':
      await lc.executeEvaluateValue()
      break;
    default:
      throw new ex.InvalidChoiceException(curCommand, possibleOptions)
  }
}

async function executeChooseDeployOptions() {
  const possibleOptions = ['Deploy contract', 'Generate deploy script']
  const curCommand = await ui.createQuickPickBox(possibleOptions, 'Deployment options', 'command')
  switch (curCommand) {
    case 'Deploy contract':
      await tc.executeDeploy()
      break;
    case 'Generate deploy script':
      await tc.executeGenerateDeployScript()
      break;
    default:
      throw new ex.InvalidChoiceException(curCommand, possibleOptions)
  }
}

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
      async () => LigoCommands.StartServer.run(client).catch(reportException),
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
      async () => LigoCommands.StopServer.run(client).catch(reportException),
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
      async () => LigoCommands.RestartServer.run(client).catch(reportException),
    ),
  },
  CompileContract: {
    name: 'ligo.compileContract',
    run: async () => lc.executeCompileContract().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileContract.name,
      async () => LigoCommands.CompileContract.run(),
    ),
  },
  CompileStorage: {
    name: 'ligo.compileStorage',
    run: async () => lc.executeCompileStorage().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileStorage.name,
      async () => LigoCommands.CompileStorage.run(),
    ),
  },
  CompileExpression: {
    name: 'ligo.compileExpression',
    run: async () => lc.executeCompileExpression().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.CompileExpression.name,
      async () => LigoCommands.CompileExpression.run(),
    ),
  },
  DryRun: {
    name: 'ligo.dryRun',
    run: async () => lc.executeDryRun().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.DryRun.name,
      async () => LigoCommands.DryRun.run(),
    ),
  },
  EvaluateFunction: {
    name: 'ligo.evaluateFunction',
    run: async () => lc.executeEvaluateFunction().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.EvaluateFunction.name,
      async () => LigoCommands.EvaluateFunction.run(),
    ),
  },
  EvaluateValue: {
    name: 'ligo.evaluateValue',
    run: async () => lc.executeEvaluateValue().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.EvaluateValue.name,
      async () => LigoCommands.EvaluateValue.run(),
    ),
  },
  Deploy: {
    name: 'ligo.deploy',
    run: async () => tc.executeDeploy().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.Deploy.name,
      async () => LigoCommands.Deploy.run(),
    ),
  },
  GenerateDeployScript: {
    name: 'ligo.generateDeployScript',
    run: async () => tc.executeGenerateDeployScript().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.GenerateDeployScript.name,
      async () => LigoCommands.GenerateDeployScript.run(),
    ),
  },
  ChooseLigoOption: {
    name: 'ligo.chooseOption',
    run: async () => executeChooseLigoOption().catch(reportException),
    register: () => vscode.commands.registerCommand(
      LigoCommands.ChooseLigoOption.name,
      async () => LigoCommands.ChooseLigoOption.run(),
    ),
  },
  ChooseDeploymentOption: {
    name: 'tezos.chooseOption',
    run:
      async () => executeChooseDeployOptions().catch(reportException),
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
