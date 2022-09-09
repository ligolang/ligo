import { ProtocolClient } from "@vscode/debugadapter-testsupport/lib/protocolClient";
import * as Net from 'net';
import {
	GetContractMetadataArguments,
	GetContractMetadataResponse,
	InitializeLoggerArguments,
	InitializeLoggerResponse,
	SetLigoBinaryPathArguments,
	SetLigoBinaryPathResponse,
	SetProgramPathArguments,
	SetProgramPathResponse,
	ValidateEntrypointArguments,
	ValidateEntrypointResponse,
	ValidateValueArguments,
	ValidateValueResponse
} from "./messages";
import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'

type LigoSpecificRequest
	= 'initializeLogger'
	| 'setLigoBinaryPath'
	| 'setProgramPath'
	| 'validateEntrypoint'
	| 'getContractMetadata'
	| 'validateValue'
	;

export default class LigoProtocolClient extends ProtocolClient {
	socket: Net.Socket
	constructor(pipeName: string) {
		super();
		this.socket = Net.createConnection({ path: pipeName }, () => {
			this.connect(this.socket, this.socket);
		});
	}

	sendMsg(command: 'initializeLogger', args: InitializeLoggerArguments): Promise<InitializeLoggerResponse>
	sendMsg(command: 'setLigoBinaryPath', args: SetLigoBinaryPathArguments) : Promise<SetLigoBinaryPathResponse>
	sendMsg(command: 'setProgramPath', args: SetProgramPathArguments): Promise<SetProgramPathResponse>
	sendMsg(command: 'validateEntrypoint', args: ValidateEntrypointArguments): Promise<ValidateEntrypointResponse>
	sendMsg(command: 'getContractMetadata', args: GetContractMetadataArguments): Promise<GetContractMetadataResponse>
	sendMsg(command: 'validateValue', args: ValidateValueArguments): Promise<ValidateValueResponse>
	sendMsg(command: LigoSpecificRequest, args: any): Promise<DebugProtocol.Response> {
		return this.send(command, args)
	}
}
