import * as fs from 'fs'
import * as vscode from 'vscode'
import { ContractMetadata, getBinaryPath, InputValueLang, isDefined, Maybe, tryExecuteCommand } from './base'
import { LigoDebugContext } from './LigoDebugContext'
import { LigoProtocolClient } from './LigoProtocolClient'
import { ValidateValueCategory } from './messages'
import { createRememberingQuickPick, getEntrypoint, getParameterOrStorage } from './ui'
import * as path from 'path'

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

	private async resolveConfig(config: vscode.DebugConfiguration): Promise<vscode.DebugConfiguration> {
		const currentWorkspacePath: Maybe<vscode.Uri> = vscode.workspace.workspaceFolders?.[0].uri;

		let defaultPath: Maybe<string>;
		if (path.isAbsolute(config.program)) {
			defaultPath = config.program;
		} else if (isDefined(currentWorkspacePath)) {
			defaultPath = vscode.Uri.joinPath(currentWorkspacePath, config.program).fsPath;
		}

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

		const pluginConfig = vscode.workspace.getConfiguration();
		const binaryPath = getBinaryPath({ name: 'ligo', path: 'ligoDebugger.ligoBinaryPath' }, pluginConfig);
		const maxSteps = pluginConfig.get<Maybe<number>>('ligoDebugger.maxSteps');
		await this.client.sendMsg('setLigoConfig', { binaryPath, maxSteps });

		const entrypoints: string[] =
			(await this.client.sendMsg('setProgramPath', { program: currentFilePath })).entrypoints.reverse();

		const entrypoint: string =
			await tryExecuteCommand(
				"entrypoint",
				"AskOnStart",
				config.entrypoint,
				() => getEntrypoint(
					this.context,
					async (entrypoint) => {
						return (await this.client.sendMsg('validateEntrypoint', { entrypoint })).message;
					},
					entrypoints
				)
			);
		config.entrypoint = entrypoint;

		const contractMetadata: ContractMetadata =
			(await this.client.sendMsg('getContractMetadata', { entrypoint })).contractMetadata;

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
			).message;
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

	async resolveDebugConfiguration(
		_folder: vscode.WorkspaceFolder | undefined,
		config: vscode.DebugConfiguration,
		_token?: vscode.CancellationToken,
	): Promise<vscode.DebugConfiguration> {
		if (!config.type && !config.request && !config.name) {
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
