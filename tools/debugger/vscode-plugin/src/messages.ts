import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'
import { ContractMetadata } from './base'

// InitializeLogger //

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

// ValidateValue //

export type ValueCategory = "parameter" | "storage" | "entrypoint"

export interface ValidateValueArguments {
	value: string
	category: ValueCategory
	pickedMichelsonEntrypoint?: string
}

export interface ValidateValueRequest extends DebugProtocol.Request {
	command: 'validateValue'
	arguments: ValidateValueArguments;
}

export interface ValidateValueResponse extends DebugProtocol.Response {
	comment: string
}

// SetFile //

export interface SetFileRequest extends DebugProtocol.Request {
	command: 'setFile'
	arguments: SetFileArguments;
}

export interface SetFileArguments {
	file: string
}

export interface SetFileResponse extends DebugProtocol.Response {
}

// SetEntrypoint //

export interface SetEntrypointRequest extends DebugProtocol.Request {
	command: 'setEntrypoint'
	arguments: SetEntrypointArguments;
}

export interface SetEntrypointArguments {
	entrypoint: string | null
}

export interface SetEntrypointResponse extends DebugProtocol.Response {
}

// GetContractMetadata //

export interface GetContractMetadataRequest extends DebugProtocol.Request {
	command: 'getContractMetadata'
	arguments: GetContractMetadataArguments;
}

export interface GetContractMetadataArguments {
}

export interface GetContractMetadataResponse extends DebugProtocol.Response {
	contractMetadata: ContractMetadata
}
