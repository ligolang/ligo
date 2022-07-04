import { ProtocolClient } from "@vscode/debugadapter-testsupport/lib/protocolClient";
import * as Net from 'net';
import {
	GetContractMetadataArguments,
	GetContractMetadataResponse,
	InitializeLoggerArguments,
	InitializeLoggerResponse,
	ValidateValueArguments,
	ValidateValueResponse
} from "./messages";
import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol'

export default class LigoProtocolClient extends ProtocolClient {
	socket: Net.Socket
	constructor(pipeName: string) {
		super();
		this.socket = Net.createConnection({ path: pipeName }, () => {
			this.connect(this.socket, this.socket);
		});
	}

	sendMsg(command: 'initializeLogger', args: InitializeLoggerArguments): Promise<InitializeLoggerResponse>
	sendMsg(command: 'getContractMetadata', args: GetContractMetadataArguments): Promise<GetContractMetadataResponse>
	sendMsg(command: 'validateValue', args: ValidateValueArguments): Promise<ValidateValueResponse>
	sendMsg(command: 'initializeLogger' | 'getContractMetadata' | 'validateValue', args: any): Promise<DebugProtocol.Response> {
		return this.send(command, args)
	}
}
