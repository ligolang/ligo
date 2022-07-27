import * as fs from 'fs'
import * as vscode from 'vscode'

function createLogDir(logDir: string): void | undefined {
	if (!fs.existsSync(logDir)) {
		fs.mkdir(logDir, { recursive: true } as fs.MakeDirectoryOptions, (err) => {
			if (err) {
				vscode.window.showErrorMessage(`Couldn't create a log directory: ${err}`)
			}
		})
	}
}

export interface AfterConfigResolvedInfo {
		file: string
		, entrypoint: string | null
	, logDir: string
}

export default class LigoDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
	private readonly afterConfigResolved: (AfterConfigResolvedInfo) => Promise<void>

	constructor(afterConfigResolved: (AfterConfigResolvedInfo) => Promise<void>) {
		this.afterConfigResolved = afterConfigResolved
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
				config.parameter = "${command:AskForParameter}"
				config.storage = "${command:AskForStorage}"
			}
		}

		// User is allowed to omit this fields, they will be assigned to the default values
		config.type ??= 'ligo'
		config.request ??= 'launch'
		config.stopOnEntry ??= true
		config.program ??= '${file}'

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

			await this.afterConfigResolved(
				{
					file: currentFilePath
					, entrypoint: config.entrypoint
					, logDir
				}
			)
		}

		return config
	}
}
