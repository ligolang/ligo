import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'

export interface InitializeLoggerRequest extends DebugProtocol.Request {
    command: 'initializeLogger'
    arguments: InitializeLoggerArguments
}

export interface InitializeLoggerArguments {
    file: string
    logDir?: string
}

export interface InitializeLoggerResponse extends DebugProtocol.Response {
}
