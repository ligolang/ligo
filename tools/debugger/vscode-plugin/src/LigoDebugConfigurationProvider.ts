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

export type InitializeLoggerAction = (file: string, logDir: string) => Promise<void>

export default class LigoDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
  private readonly initializeLogger: InitializeLoggerAction

  constructor(initializeLogger: InitializeLoggerAction) {
    this.initializeLogger = initializeLogger
  }

  resolveDebugConfiguration(
    _folder: vscode.WorkspaceFolder | undefined,
    config: vscode.DebugConfiguration,
    _token?: vscode.CancellationToken,
  ): vscode.ProviderResult<vscode.DebugConfiguration> {
    if (!config.type && !config.request && !config.name) {
      const editor = vscode.window.activeTextEditor
      if (editor && (editor.document.languageId === 'ligo' || editor.document.languageId === 'mligo' || editor.document.languageId === 'religo' || editor.document.languageId === 'jsligo')) {
        config.type = 'ligo'
        config.name = 'Launch LIGO'
        config.request = 'launch'
        config.program = '${file}'
        config.stopOnEntry = true
      }
    }

    if (config.logDir === '') {
      config.logDir = undefined
    }

    if (!config.program) {
      return vscode.window.showErrorMessage('Cannot find a program to debug').then((_) => undefined)
    }

    const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
    if (currentFilePath) {
      const { logDir } = config
      // Create logDir if needed
      if (logDir) {
        createLogDir(logDir)
      }

      this.initializeLogger(currentFilePath, logDir)
    }

    return config
  }
}
