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

export type ValidateValueCategory = "parameter" | "storage"

export interface ValidateValueArguments {
	value: string
	category: ValidateValueCategory
	pickedMichelsonEntrypoint?: string
}

export interface ValidateValueRequest extends DebugProtocol.Request {
	command: 'validateValue'
	arguments: ValidateValueArguments;
}

export interface ValidateValueResponse extends DebugProtocol.Response {
	comment: string
}

// GetContractMetadata //

export interface GetContractMetadataRequest extends DebugProtocol.Request {
	command: 'getContractMetadata'
	arguments: GetContractMetadataArguments;
}

export interface GetContractMetadataArguments {
	file: string
	entrypoint: string | null
}

export interface GetContractMetadataResponse extends DebugProtocol.Response {
	contractMetadata: ContractMetadata
}
