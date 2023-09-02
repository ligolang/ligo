import * as fs from 'fs'
import * as vscode from 'vscode'
import { ContractMetadata, generatedEntrypointName, getBinaryPath, InputValueLang, isDefined, Maybe, tryExecuteCommand } from './base'
import { LigoDebugContext } from './LigoDebugContext'
import { LigoProtocolClient } from './LigoProtocolClient'
import { ValidateValueCategory } from './messages'
import { createConfigSnippet, createRememberingQuickPick, getConfigPath, getEntrypoint, getParameterOrStorage } from './ui'

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
  = "entrypoint"
  | "michelsonEntrypoint"
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
  private context: LigoDebugContext;

  constructor(client: LigoProtocolClient, context: LigoDebugContext) {
    this.client = client;
    this.context = context;
  }

  private async tryToResolveConfigFromLigo(config: vscode.DebugConfiguration): Promise<vscode.DebugConfiguration> {
    const pluginConfig = vscode.workspace.getConfiguration();
    const binaryPath = getBinaryPath({ name: 'ligo', path: 'ligoLanguageServer.ligoBinaryPath' }, pluginConfig);
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
      config.michelsonEntrypoint = resolvedConfig.michelsonEntrypoint;
      config.entrypoint = resolvedConfig.entrypoint ?? "(*@AskOnStart@*)";
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

    const entrypoints: [string, string][] =
      (await this.client.sendMsg('setProgramPath', { program: currentFilePath })).body.entrypoints.reverse();

    const entrypoint: string =
      await tryExecuteCommand(
        "entrypoint",
        "AskOnStart",
        config.entrypoint,
        () => getEntrypoint(
          this.context,
          async (entrypoint) => {
            return (await this.client.sendMsg('validateEntrypoint', { entrypoint })).body?.errorMessage;
          },
          entrypoints
        ),
        generatedEntrypointName(config.entrypoint),
      );
    config.entrypoint = entrypoint;

    const contractMetadata: ContractMetadata =
      (await this.client.sendMsg('getContractMetadata', { entrypoint })).body;

    const michelsonEntrypoint: Maybe<string> =
      await tryExecuteCommand(
        "michelsonEntrypoint",
        "AskOnStart",
        config.michelsonEntrypoint as Maybe<string>,
        () => createRememberingQuickPick(
          contractMetadata,
          "Please pick a Michelson entrypoint to run"
        )
      );
    config.michelsonEntrypoint = michelsonEntrypoint;

    const validateInput = (category: ValidateValueCategory, valueLang: InputValueLang) => async (value: string): Promise<Maybe<string>> => {
      return (
        await this.client.sendMsg(
          'validateValue',
          { value, category, valueLang, pickedMichelsonEntrypoint: michelsonEntrypoint }
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
          entrypoint,
          contractMetadata,
          michelsonEntrypoint
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
          entrypoint,
          contractMetadata,
          michelsonEntrypoint
        ),
        [config.storage, config.storageLang]
      );
    config.storage = storage;
    config.storageLang = storageLang;

    await this.client.sendMsg('validateConfig', { michelsonEntrypoint, parameter, parameterLang, storage, storageLang });
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
    if (this.configDoesntExist(config)) {
      const editor = vscode.window.activeTextEditor
      if (editor && (editor.document.languageId === 'mligo' || editor.document.languageId === 'jsligo')) {
        config.type = 'ligo'
        config.name = 'Launch LIGO'
        config.request = 'launch'
        config.program = '(*@CurrentFile@*)'
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
