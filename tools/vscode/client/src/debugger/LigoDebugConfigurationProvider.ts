import * as fs from 'fs'
import * as vscode from 'vscode'
import { ContractMetadata, generatedModuleName, interruptExecution, tryExecuteCommand } from './base'
import { LigoContext } from '../common/LigoContext'
import { LigoProtocolClient, showErrorWithOpenLaunchJson } from '../common/LigoProtocolClient'
import { ValidateValueCategory } from './messages'
import { createConfigSnippet, createRememberingQuickPick, getConfigPath, getModuleName, getParameterOrStorage } from './ui'
import { InputValueLang, isDefined, Maybe } from '../common/base'
import { getBinaryPath, ligoBinaryInfo } from '../common/config'

function createLogDir(logDir: string): void | undefined {
  if (!fs.existsSync(logDir)) {
    fs.mkdir(logDir, { recursive: true } as fs.MakeDirectoryOptions, (err) => {
      if (err) {
        vscode.window.showErrorMessage(`Couldn't create a log directory: ${err}`)
      }
    })
  }
}

export type ConfigField
  = "moduleName"
  | "entrypoint"
  | "parameter"
  | "storage"
  | "program"
  | "configPath"
  ;

export type ConfigCommand
  = "AskOnStart"
  | "CurrentFile"
  ;

export default class LigoDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
  private client: LigoProtocolClient;
  private context: LigoContext;

  constructor(client: LigoProtocolClient, context: LigoContext) {
    this.client = client;
    this.context = context;
  }

  private async tryToResolveConfigFromLigo(config: vscode.DebugConfiguration): Promise<vscode.DebugConfiguration> {
    const binaryPath = getBinaryPath(ligoBinaryInfo);
    const pluginConfig = vscode.workspace.getConfiguration();
    const maxSteps = pluginConfig.get<Maybe<number>>('ligoDebugger.maxSteps');
    await this.client.sendMsg('setLigoConfig', { binaryPath, maxSteps });

    const configPath: Maybe<string> =
      await tryExecuteCommand(
        "configPath",
        "AskOnStart",
        config.configPath,
        () => getConfigPath(this.context),
        config.configPath,
        false,
      );

    if (isDefined(configPath)) {
      const resolvedConfig =
        (await this.client.sendMsg('resolveConfigFromLigo', { configPath })).body;

      config.program = resolvedConfig.program ?? "(*@CurrentFile@*)";
      config.entrypoint = resolvedConfig.entrypoint ?? "(*@AskOnStart@*)";
      config.moduleName = resolvedConfig.moduleName ?? "(*@AskOnStart@*)";
      config.contractEnv = resolvedConfig.contractEnv;

      if (isDefined(resolvedConfig.parameter)) {
        config.parameter = resolvedConfig.parameter;
        config.parameterLang = "Michelson";
      } else {
        config.parameter = "(*@AskOnStart@*)";
      }

      if (isDefined(resolvedConfig.storage)) {
        config.storage = resolvedConfig.storage;
        config.storageLang = "Michelson";
      } else {
        config.storage = "(*@AskOnStart@*)";
      }
    }

    return config;
  }

  private async resolveConfig(config: vscode.DebugConfiguration): Promise<vscode.DebugConfiguration> {
    const defaultPath: Maybe<string> = config.program;

    const currentFilePath: Maybe<string> =
      await tryExecuteCommand(
        "program",
        "CurrentFile",
        config.program,
        async () => vscode.window.activeTextEditor?.document.fileName,
        defaultPath
      );

    if (!isDefined(currentFilePath)) {
      throw new Error("Can't find current file");
    }

    await this.client.sendMsg('initializeLogger', { file: currentFilePath, logDir: config.logDir });

    const moduleNames: [string, string][] =
      (await this.client.sendMsg('setProgramPath', { program: currentFilePath })).body.moduleNames.reverse();

    const moduleName: string =
      await tryExecuteCommand(
        "moduleName",
        "AskOnStart",
        config.moduleName,
        () => getModuleName(
          this.context,
          async (moduleName) => {
            return (await this.client.sendMsg('validateModuleName', { moduleName })).body?.errorMessage;
          },
          moduleNames
        ),
        generatedModuleName(config.moduleName),
      );
    config.moduleName = moduleName;

    const contractMetadata: ContractMetadata =
      (await this.client.sendMsg('getContractMetadata', { moduleName })).body;

    const entrypoint: string =
      await tryExecuteCommand(
        "entrypoint",
        "AskOnStart",
        config.entrypoint,
        () => createRememberingQuickPick(
          contractMetadata,
          "Please pick an entrypoint to run"
        )
      );

    const entrypointValidateResult =
      await this.client.sendMsg('validateEntrypoint', { pickedEntrypoint: entrypoint });

    if (isDefined(entrypointValidateResult.body)) {
      showErrorWithOpenLaunchJson(entrypointValidateResult.body.errorMessage);
      interruptExecution();
    }

    config.entrypoint = entrypoint;

    const validateInput = (category: ValidateValueCategory, valueLang: InputValueLang) => async (value: string): Promise<Maybe<string>> => {
      return (
        await this.client.sendMsg(
          'validateValue',
          { value, category, valueLang }
        )
      ).body?.errorMessage;
    }

    const [parameter, parameterLang]: [string, InputValueLang] =
      await tryExecuteCommand(
        "parameter",
        "AskOnStart",
        config.parameter,
        () => getParameterOrStorage(
          this.context,
          validateInput,
          "parameter",
          "Input the contract parameter",
          "Parameter value",
          moduleName,
          contractMetadata,
          entrypoint
        ),
        [config.parameter, config.parameterLang]
      );
    config.parameter = parameter;
    config.parameterLang = parameterLang;

    const [storage, storageLang]: [string, InputValueLang] =
      await tryExecuteCommand(
        "storage",
        "AskOnStart",
        config.storage,
        () => getParameterOrStorage(
          this.context,
          validateInput,
          "storage",
          "Input the contract storage",
          "Storage value",
          moduleName,
          contractMetadata,
          entrypoint
        ),
        [config.storage, config.storageLang]
      );
    config.storage = storage;
    config.storageLang = storageLang;

    await this.client.sendMsg('validateConfig', { parameter, parameterLang, storage, storageLang });
    return config;
  }

  private configDoesntExist(config: vscode.DebugConfiguration): boolean {
    return !config.type && !config.request && !config.name;
  }

  async resolveDebugConfiguration(
    _folder: vscode.WorkspaceFolder | undefined,
    config: vscode.DebugConfiguration,
    _token?: vscode.CancellationToken,
  ): Promise<vscode.DebugConfiguration> {
    // Binary path would be passed later
    await this.client.sendMsg('initializeLanguageServerState', { binaryPath: null });

    if (this.configDoesntExist(config)) {
      const editor = vscode.window.activeTextEditor
      if (editor && (editor.document.languageId === 'mligo' || editor.document.languageId === 'jsligo')) {
        config.type = 'ligo'
        config.name = 'Launch LIGO'
        config.request = 'launch'
        config.program = '(*@CurrentFile@*)'
        config.moduleName = '(*@AskOnStart@*)'
        config.entrypoint = '(*@AskOnStart@*)'
        config.parameter = '(*@AskOnStart@*)'
        config.storage = '(*@AskOnStart@*)'
      }
    } else if (!isDefined(config.configPath)) {
      const askedForConfig = this.context.workspaceState.askedForLigoConfig();
      if (!isDefined(askedForConfig) || !askedForConfig.value) {
        vscode.window.showInformationMessage(
          'LIGO configuration not found. Create a launch configuration file?',
          'Yes',
        ).then(async result => {
          if (isDefined(result)) {
            const created = await createConfigSnippet(this.context);
            askedForConfig.value = created;
          }
        });
      }
    }

    // User is allowed to omit this fields, they will be assigned to the default values
    config.type ??= 'ligo'
    config.request ??= 'launch'
    config.stopOnEntry ??= true
    config.program ??= '(*@CurrentFile@*)'
    config.moduleName ??= '(*@AskOnStart@*)'
    config.entrypoint ??= '(*@AskOnStart@*)'
    config.parameterLang ??= 'LIGO'
    config.storageLang ??= 'LIGO'

    if (config.logDir === '') {
      config.logDir = undefined
    }

    const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
    if (currentFilePath) {
      config = await this.tryToResolveConfigFromLigo(config);

      const { logDir } = config
      // Create logDir if needed
      if (logDir) {
        createLogDir(logDir)
      }

      config = await this.resolveConfig(config);
    }

    return config
  }
}
