import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'
import { InputValueLang } from '../common/base'
import { ContractMetadata, LigoLaunchRequest } from './base'
import { SteppingGranularity } from './ui'

// ResolveConfigFromLigo //

/** A message from a request indicating to resolve a configuration from LIGO. */
export interface ResolveConfigFromLigoRequest extends DebugProtocol.Request {
  command: 'resolveConfigFromLigo'
  arguments: ResolveConfigFromLigoArguments
}

/** Arguments of the {@link ResolveConfigFromLigoRequest}. */
export interface ResolveConfigFromLigoArguments {
  /** A path to the LIGO file containing the debugger configuration. */
  configPath: string
}

/** Response of the {@link ResolveConfigFromLigoRequest}. */
export interface ResolveConfigFromLigoResponse extends DebugProtocol.Response {
  body: LigoLaunchRequest
}

// InitializeLanguageServerState //

/**
 * A message from a request to intialize the language server state from
 * LIGO Debugger. This is not related to LIGO LSP.
 */
export interface InitializeLanguageServerStateRequest extends DebugProtocol.Request {
  command: 'initializeLanguageServerState'
  arguments: InitializeLanguageServerStateArguments
}

/** Arguments of the {@link InitializeLanguageServerStateRequest}. */
export interface InitializeLanguageServerStateArguments {
  /** A file path to the LIGO compiler. */
  binaryPath?: string
}

/** Response of the {@link InitializeLanguageServerStateRequest}. */
export interface InitializeLanguageServerStateResponse extends DebugProtocol.Response {
}

// InitializeLogger //

/** A request to initialize the logger. */
export interface InitializeLoggerRequest extends DebugProtocol.Request {
  command: 'initializeLogger'
  arguments: InitializeLoggerArguments
}

/** Arguments of the {@link InitializeLoggerRequest}. */
export interface InitializeLoggerArguments {
  /** Name of the log file. */
  file: string

  /** The directory to be used for logging. */
  logDir?: string
}

/** Response of the {@link InitializeLoggerRequest}. */
export interface InitializeLoggerResponse extends DebugProtocol.Response {
}

// SetLigoConfig //

/** A request to set configuration arguments. */
export interface SetLigoConfigArguments {
  /** A path to the LIGO binary. */
  binaryPath?: string

  /**
   * The maximum number of steps before the debugging is interrupted. An empty
   * value indicates infinite steps.
   */
  maxSteps?: number
}

/** Arguments of the {@link SetLigoConfigRequest}. */
export interface SetLigoConfigRequest extends DebugProtocol.Request {
  command: 'setLigoConfig'
  arguments: SetLigoConfigArguments
}

/** Response of the {@link SetLigoConfigRequest}. */
export interface SetLigoConfigResponse extends DebugProtocol.Response {
}

// SetProgramPath //

/** A request to set the path to the contract being debugged. */
export interface SetProgramPathRequest extends DebugProtocol.Request {
  command: 'setProgramPath'
  arguments: SetProgramPathArguments
}

/** Response of the {@link SetProgramPathArguments}. */
export interface SetProgramPathArguments {
  /** The path to the contract being debugged. */
  program: string
}

/** Response of the {@link SetProgramPathRequest}. */
export interface SetProgramPathResponse extends DebugProtocol.Response {
  body: {
    moduleNames: [string, string][]
  }
}

// ValidateModuleName //

/** A request to validate the module name in the contract being debugged. */
export interface ValidateModuleNameRequest extends DebugProtocol.Request {
  command: 'validateModuleName'
  arguments: ValidateModuleNameArguments
}

/** Arguments of the {@link ValidateModuleNameArgumentsRequest}. */
export interface ValidateModuleNameArguments {
  /** The module path to be validated. */
  moduleName: string
}

/** Response of the {@link ValidateModuleNameArgumentsRequest}. */
export interface ValidateResponse extends DebugProtocol.Response {
  body?: {
    errorMessage: string
  }
}

// ValidateValue //

/** Whether to validate a parameter value or storage value. */
export type ValidateValueCategory = "parameter" | "storage"

/** A request to validate some user-provided value. */
export interface ValidateValueRequest extends DebugProtocol.Request {
  command: 'validateValue'
  arguments: ValidateValueArguments;
}

/** Arguments of the {@link ValidateValueRequest}. */
export interface ValidateValueArguments {
  /** The value to be validated. */
  value: string

  /** Whether the value is a parameter or storage. */
  category: ValidateValueCategory

  /** Whether to interpret the value in LIGO or Michelson syntax. */
  valueLang: InputValueLang
}

/** Response of the {@link ValidateValueRequest}. */
export interface ValidateValueResponse extends DebugProtocol.Response {
  body?: {
    errorMessage: string
  }
}

// GetContractMetadata //

/** A request to fetch a contract's metadata. */
export interface GetContractMetadataRequest extends DebugProtocol.Request {
  command: 'getContractMetadata'
  arguments: GetContractMetadataArguments;
}

/** Arguments of the {@link GetContractMetadataRequest}. */
export interface GetContractMetadataArguments {
  /** The module path to which the contract metadata should be fetched. */
  moduleName: string
}

/** Response of the {@link GetContractMetadataRequest}. */
export interface GetContractMetadataResponse extends DebugProtocol.Response {
  body: ContractMetadata
}

// ValidateEntrypoint //

/** A request to validate an entry-point. */
export interface ValidateEntrypointRequest extends DebugProtocol.Request {
  command: 'validateEntrypoint'
  arguments: ValidateEntrypointArguments
}

/** Arguments of the {@link ValidateEntrypointRequest}. */
export interface ValidateEntrypointArguments {
  /** The entry point name to be validated. */
  pickedEntrypoint: string
}

// ValidateConfig //

/** A request to validate a LIGO configuration. */
export interface ValidateConfigRequest extends DebugProtocol.Request {
  command: 'validateConfig'
  arguments: ValidateConfigArguments
}

/** Arguments of the {@link ValidateConfigRequest}. */
export interface ValidateConfigArguments {
  /** Parameter value to be validated. */
  parameter: string

  /**
   * Whether to validate the input parameter interpreting it as a Michelson or
   * LIGO value.
   */
  parameterLang: InputValueLang

  /** Storage value to be validated. */
  storage: string

  /**
   * Whether to validate the input storage interpreting it as a Michelson or
   * LIGO value.
   */
  storageLang: InputValueLang
}

/** Response of the {@link ValidateConfigRequest}. */
export interface ValidateConfigResponse extends DebugProtocol.Response {
}

// SetSteppingGranularity //

export interface SetSteppingGranularityRequest extends DebugProtocol.Request {
  command: 'SetSteppingGranularity'
  arguments: SetSteppingGranularityArguments;
}

/** Arguments of the {@link SetSetSteppingGranularityRequest}. */
export interface SetSteppingGranularityArguments {
  /** The chosen stepping granularity. */
  granularity: SteppingGranularity
}

/** Response of the {@link SetSetSteppingGranularityRequest}. */
export interface SetSteppingGranularityResponse extends DebugProtocol.Response {
}
