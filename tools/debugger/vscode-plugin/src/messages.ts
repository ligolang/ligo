import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'
import { ContractMetadata } from './base'
import { InputValueType } from './ui'

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

// SetLigoBinaryPath //

export interface SetLigoBinaryPathArguments {
	binaryPath?: string
}

export interface SetLigoBinaryPathRequest extends DebugProtocol.Request {
	command: 'setLigoBinaryPath'
	arguments: SetLigoBinaryPathArguments
}

export interface SetLigoBinaryPathResponse extends DebugProtocol.Response {
}

// SetProgramPath //

export interface SetProgramPathRequest extends DebugProtocol.Request {
	command: 'setProgramPath'
	arguments: SetProgramPathArguments
}

export interface SetProgramPathArguments {
	program: string
}

export interface SetProgramPathResponse extends DebugProtocol.Response {
	entrypoints: [string]
}

// ValidateEntrypoint //

export interface ValidateEntrypointRequest extends DebugProtocol.Request {
	command: 'validateEntrypoint'
	arguments: ValidateEntrypointArguments
}

export interface ValidateEntrypointArguments {
	entrypoint: string
}

export interface ValidateEntrypointResponse extends DebugProtocol.Response {
	message?: string
}

// ValidateValue //

export type ValidateValueCategory = "parameter" | "storage"

export interface ValidateValueArguments {
	value: string
	category: ValidateValueCategory
	valueType: InputValueType
	pickedMichelsonEntrypoint?: string
}

export interface ValidateValueRequest extends DebugProtocol.Request {
	command: 'validateValue'
	arguments: ValidateValueArguments;
}

export interface ValidateValueResponse extends DebugProtocol.Response {
	message?: string
}

// GetContractMetadata //

export interface GetContractMetadataRequest extends DebugProtocol.Request {
	command: 'getContractMetadata'
	arguments: GetContractMetadataArguments;
}

export interface GetContractMetadataArguments {
	entrypoint: string | null
}

export interface GetContractMetadataResponse extends DebugProtocol.Response {
	contractMetadata: ContractMetadata
}

// ValidateConfig //

export interface ValidateConfigArguments {
	michelsonEntrypoint?: string
	parameter: string
	storage: string
}

export interface ValidateConfigRequest extends DebugProtocol.Request {
	command: 'validateConfig'
	arguments: ValidateConfigArguments
}

export interface ValidateConfigResponse extends DebugProtocol.Response {
}
