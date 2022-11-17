import * as fs from 'fs'
import * as vscode from 'vscode'
import { ContractMetadata, getBinaryPath, InputValueType, isDefined, Maybe, tryExecuteCommand } from './base'
import { LigoDebugContext } from './LigoDebugContext'
import LigoProtocolClient from './LigoProtocolClient'
import { ValidateValueCategory } from './messages'
import { createRememberingQuickPick, getEntrypoint, getParameterOrStorage } from './ui'

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
	;

export type ConfigCommand
	= "AskOnStart"
	;

export default class LigoDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
	private client : LigoProtocolClient;
	private context : LigoDebugContext;

	constructor(client: LigoProtocolClient, context: LigoDebugContext) {
		this.client = client;
		this.context = context;
	}

	private async resolveConfig(config: vscode.DebugConfiguration) : Promise<vscode.DebugConfiguration> {
		const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath;

		if (!isDefined(currentFilePath)) {
			throw new Error("Can't find current file");
		}

		await this.client.sendMsg('initializeLogger', { file: currentFilePath, logDir: config.logDir });

		const pluginConfig = vscode.workspace.getConfiguration();
		const binaryPath = getBinaryPath({ name: 'ligo', path: 'ligoDebugger.ligoBinaryPath' }, pluginConfig);
		await this.client.sendMsg('setLigoBinaryPath', { binaryPath });

		const entrypoints : string[] =
			(await this.client.sendMsg('setProgramPath', { program: currentFilePath })).entrypoints.reverse();

		const entrypoint : string =
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

		const contractMetadata : Maybe<ContractMetadata> =
			(await this.client.sendMsg('getContractMetadata', { entrypoint })).contractMetadata;

		if (!isDefined(contractMetadata)) {
			return config;
		}

		const michelsonEntrypoint : string =
			await tryExecuteCommand(
				"michelsonEntrypoint",
				"AskOnStart",
				config.michelsonEntrypoint,
				() => createRememberingQuickPick(
					contractMetadata,
					"Please pick a Michelson entrypoint to run"
				)
			);
		config.michelsonEntrypoint = michelsonEntrypoint;

		const validateInput = (category: ValidateValueCategory, valueType: InputValueType) => async (value: string): Promise<Maybe<string>> => {
			return (
				await this.client.sendMsg(
					'validateValue',
					{ value, category, valueType, pickedMichelsonEntrypoint: michelsonEntrypoint }
				)
			).message;
		}

		const parameter : string =
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
				)
			);
		config.parameter = parameter;

		const storage : string =
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
				)
			);
		config.storage = storage;

		return config;
	}

	async resolveDebugConfiguration(
		_folder: vscode.WorkspaceFolder | undefined,
		config: vscode.DebugConfiguration,
		_token?: vscode.CancellationToken,
	): Promise<vscode.DebugConfiguration> {
		if (!config.type && !config.request && !config.name) {
			const editor = vscode.window.activeTextEditor
			if (editor && (editor.document.languageId === 'ligo' || editor.document.languageId === 'mligo' || editor.document.languageId === 'religo' || editor.document.languageId === 'jsligo')) {
				config.type = 'ligo'
				config.name = 'Launch LIGO'
				config.request = 'launch'
				config.program = '${file}'
				config.entrypoint = '{AskOnStart}'
				config.parameter = "{AskOnStart}"
				config.storage = "{AskOnStart}"
			}
		}

		// User is allowed to omit this fields, they will be assigned to the default values
		config.type ??= 'ligo'
		config.request ??= 'launch'
		config.stopOnEntry ??= true
		config.program ??= '${file}'
		config.entrypoint ??= '{AskOnStart}'

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
