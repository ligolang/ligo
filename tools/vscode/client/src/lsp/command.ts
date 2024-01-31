import * as vscode from 'vscode'

import {
  LanguageClient,
} from 'vscode-languageclient/node'
import { ligoOutput } from './commands/common'

import * as lc from './commands/ligoCommands'
import * as tc from './commands/tezosCommands'
import * as ui from './ui'
import * as ex from '../common/exceptions'
import { LigoContext } from '../common/LigoContext'
import { LigoProtocolClient } from '../common/LigoProtocolClient'

function reportException(exception: Error) {
  if (!(exception instanceof ex.UserInterruptionException)) {
    vscode.window.showErrorMessage('Exception happened during execution.\n'
      + 'See console (LIGO Compiler) for additional information', 'Ok')
    ligoOutput.appendLine(exception.message)
    ligoOutput.show()
  }
}

async function executeChooseLigoOption(context: LigoContext, client: LigoProtocolClient) {
  const possibleOptions = ['Compile contract', 'Compile storage', 'Compile expression', 'Dry run', 'Evaluate function', 'Evaluate value']
  const curCommand = await ui.createQuickPickBox(possibleOptions, 'ligo compiler command', 'command')
  switch (curCommand) {
    case 'Compile contract':
      await lc.executeCompileContract(context, client)
      break;
    case 'Compile storage':
      await lc.executeCompileStorage(context, client)
      break;
    case 'Compile expression':
      await lc.executeCompileExpression()
      break;
    case 'Dry run':
      await lc.executeDryRun(context, client)
      break;
    case 'Evaluate function':
      await lc.executeEvaluateFunction(context)
      break;
    case 'Evaluate value':
      await lc.executeEvaluateValue(context)
      break;
    default:
      throw new ex.InvalidChoiceException(curCommand, possibleOptions)
  }
}

async function executeChooseDeployOptions(context: LigoContext, client: LigoProtocolClient) {
  const possibleOptions = ['Deploy contract', 'Generate deploy script']
  const curCommand = await ui.createQuickPickBox(possibleOptions, 'Deployment options', 'command')
  switch (curCommand) {
    case 'Deploy contract':
      await tc.executeDeploy(context, client)
      break;
    case 'Generate deploy script':
      await tc.executeGenerateDeployScript(context, client)
      break;
    default:
      throw new ex.InvalidChoiceException(curCommand, possibleOptions)
  }
}

const LigoCommands = {
  StartServer: {
    name: 'ligo.startServer',
    run: async (client: LanguageClient, semanticTokensClient: LanguageClient) => {
      client.info('Starting LIGO LSP server')
      await client.start()
      await semanticTokensClient.start()
      client.info('Started LIGO LSP server')
    },
    register: (client: LanguageClient, semanticTokensClient: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.StartServer.name,
      async () => LigoCommands.StartServer.run(client, semanticTokensClient).catch(reportException),
    ),
  },
  StopServer: {
    name: 'ligo.stopServer',
    run: async (client: LanguageClient, semanticTokensClient: LanguageClient) => {
      client.info('Stopping LIGO LSP server')
      await client.stop()
      await semanticTokensClient.stop()
      client.info('Stopped LIGO LSP server')
    },
    register: (client: LanguageClient, semanticTokensClient: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.StopServer.name,
      async () => LigoCommands.StopServer.run(client, semanticTokensClient).catch(reportException),
    ),
  },
  RestartServer: {
    name: 'ligo.restartServer',
    run: async (client: LanguageClient, semanticTokensClient: LanguageClient) => {
      await LigoCommands.StopServer.run(client, semanticTokensClient)
      await LigoCommands.StartServer.run(client, semanticTokensClient)
    },
    register: (client: LanguageClient, semanticTokensClient: LanguageClient) => vscode.commands.registerCommand(
      LigoCommands.RestartServer.name,
      async () => LigoCommands.RestartServer.run(client, semanticTokensClient).catch(reportException),
    ),
  },
  CompileContract: {
    name: 'ligo.compileContract',
    run: async (context: LigoContext, client: LigoProtocolClient) =>
      lc.executeCompileContract(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.CompileContract.name,
      async () => LigoCommands.CompileContract.run(context, client),
    ),
  },
  CompileStorage: {
    name: 'ligo.compileStorage',
    run: async (context: LigoContext, client: LigoProtocolClient) =>
      lc.executeCompileStorage(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.CompileStorage.name,
      async () => LigoCommands.CompileStorage.run(context, client),
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
    run: async (context: LigoContext, client: LigoProtocolClient) =>
      lc.executeDryRun(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.DryRun.name,
      async () => LigoCommands.DryRun.run(context, client),
    ),
  },
  EvaluateFunction: {
    name: 'ligo.evaluateFunction',
    run: async (context: LigoContext) => lc.executeEvaluateFunction(context).catch(reportException),
    register: (context: LigoContext) => vscode.commands.registerCommand(
      LigoCommands.EvaluateFunction.name,
      async () => LigoCommands.EvaluateFunction.run(context),
    ),
  },
  EvaluateValue: {
    name: 'ligo.evaluateValue',
    run: async (context: LigoContext) => lc.executeEvaluateValue(context).catch(reportException),
    register: (context: LigoContext) => vscode.commands.registerCommand(
      LigoCommands.EvaluateValue.name,
      async () => LigoCommands.EvaluateValue.run(context),
    ),
  },
  Deploy: {
    name: 'ligo.deploy',
    run: async (context: LigoContext, client: LigoProtocolClient) =>
      tc.executeDeploy(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.Deploy.name,
      async () => LigoCommands.Deploy.run(context, client),
    ),
  },
  GenerateDeployScript: {
    name: 'ligo.generateDeployScript',
    run: async (context: LigoContext, client: LigoProtocolClient) =>
      tc.executeGenerateDeployScript(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.GenerateDeployScript.name,
      async () => LigoCommands.GenerateDeployScript.run(context, client),
    ),
  },
  ChooseLigoOption: {
    name: 'ligo.chooseOption',
    run: async (context: LigoContext, client: LigoProtocolClient) =>
      executeChooseLigoOption(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.ChooseLigoOption.name,
      async () => LigoCommands.ChooseLigoOption.run(context, client),
    ),
  },
  ChooseDeploymentOption: {
    name: 'tezos.chooseOption',
    run:
      async (context: LigoContext, client: LigoProtocolClient) =>
        executeChooseDeployOptions(context, client).catch(reportException),
    register: (context: LigoContext, client: LigoProtocolClient) => vscode.commands.registerCommand(
      LigoCommands.ChooseDeploymentOption.name,
      async () => LigoCommands.ChooseDeploymentOption.run(context, client),
    ),
  },
}

export default LigoCommands

export function registerCommands(
  context: LigoContext,
  client: LanguageClient,
  semanticTokensClient: LanguageClient,
  protocolClient: LigoProtocolClient
) {
  // Boot
  LigoCommands.StartServer.register(client, semanticTokensClient);
  LigoCommands.StopServer.register(client, semanticTokensClient);
  LigoCommands.RestartServer.register(client, semanticTokensClient);
  // Buttons
  LigoCommands.ChooseLigoOption.register(context, protocolClient);
  LigoCommands.ChooseDeploymentOption.register(context, protocolClient);
  // LIGO commands
  LigoCommands.CompileContract.register(context, protocolClient);
  LigoCommands.CompileExpression.register();
  LigoCommands.CompileStorage.register(context, protocolClient);
  LigoCommands.DryRun.register(context, protocolClient);
  LigoCommands.EvaluateFunction.register(context);
  LigoCommands.EvaluateValue.register(context);
  // Tezos commands
  LigoCommands.Deploy.register(context, protocolClient);
  LigoCommands.GenerateDeployScript.register(context, protocolClient);
}
