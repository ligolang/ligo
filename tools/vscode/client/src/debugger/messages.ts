import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'
import { ContractMetadata, InputValueLang, LigoLaunchRequest } from './base'
import { SteppingGranularity } from './ui'

// ResolveConfigFromLigo //

export interface ResolveConfigFromLigoRequest extends DebugProtocol.Request {
  command: 'resolveConfigFromLigo'
  arguments: ResolveConfigFromLigoArguments
}

export interface ResolveConfigFromLigoArguments {
  configPath: string
}

export interface ResolveConfigFromLigoResponse extends DebugProtocol.Response {
  body: LigoLaunchRequest
}

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

// SetLigoConfig //

export interface SetLigoConfigArguments {
  binaryPath?: string
  maxSteps?: number
}

export interface SetLigoConfigRequest extends DebugProtocol.Request {
  command: 'setLigoConfig'
  arguments: SetLigoConfigArguments
}

export interface SetLigoConfigResponse extends DebugProtocol.Response {
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
  body: {
    entrypoints: [string, string][]
  }
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
  body?: {
    errorMessage: string
  }
}

// ValidateValue //

export type ValidateValueCategory = "parameter" | "storage"

export interface ValidateValueArguments {
  value: string
  category: ValidateValueCategory
  valueLang: InputValueLang
  pickedMichelsonEntrypoint?: string
}

export interface ValidateValueRequest extends DebugProtocol.Request {
  command: 'validateValue'
  arguments: ValidateValueArguments;
}

export interface ValidateValueResponse extends DebugProtocol.Response {
  body?: {
    errorMessage: string
  }
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
  body: ContractMetadata
}

// ValidateConfig //

export interface ValidateConfigArguments {
  michelsonEntrypoint?: string
  parameter: string
  parameterLang: InputValueLang
  storage: string
  storageLang: InputValueLang
}

export interface ValidateConfigRequest extends DebugProtocol.Request {
  command: 'validateConfig'
  arguments: ValidateConfigArguments
}

export interface ValidateConfigResponse extends DebugProtocol.Response {
}

// SetSteppingGranularity //

export interface SetSteppingGranularityArguments {
  granularity: SteppingGranularity
}

export interface SetSteppingGranularityRequest extends DebugProtocol.Request {
  command: 'SetSteppingGranularity'
  arguments: SetSteppingGranularityArguments;
}

export interface SetSteppingGranularityResponse extends DebugProtocol.Response {
}
