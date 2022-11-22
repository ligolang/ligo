// SPDX-FileCopyrightText: 2022 Microsoft
// SPDX-License-Identifier: LicenseRef-MIT-Microsoft

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
import stream from 'stream'
import * as ee from 'events'
import * as vscode from 'vscode'
import { isDefined } from './base';
import { version } from 'process';

type LigoSpecificRequest
	= 'initializeLogger'
	| 'setLigoBinaryPath'
	| 'setProgramPath'
	| 'validateEntrypoint'
	| 'getContractMetadata'
	| 'validateValue'
	;

/**
 * Make up a large text for an error.
 *
 * It will be marked to appear in a modal dialog box as other options do
 * not allow for showing large texts conveniently.
 */
function largeError(...messageParts: string[]): vscode.MessageOptions {
	return { modal: true, detail: messageParts.join("\n\n") }
}

function processErrorResponse(response: DebugProtocol.ErrorResponse): void {
	if (!isDefined(response.body.error)) {
		return
	}

	let formattedMessage = response.body.error.format;

	// Special exception types
	switch (response.message) {
		case "UnsupportedLigoVersion":
			vscode.window.showWarningMessage(formattedMessage, "Open latest supported release page")
				.then(answer => {
					if (isDefined(answer)) {
						let latestSupported = response.body.error?.variables?.recommendedVersion;
						if (!isDefined(latestSupported)) {
							latestSupported = "";
						}

						vscode.env.openExternal(vscode.Uri.parse("https://gitlab.com/ligolang/ligo/-/releases/" + latestSupported))
							.then(result => {
								if (!result) {
									vscode.window.showErrorMessage("Failed to open LIGO releases page.");
								}

								return result;
							});
					}
				});
			return;
	}

	// Handling exception origins
	switch (response.body.error?.variables?.origin) {
		case "user":
			vscode.window.showErrorMessage("Error", largeError(formattedMessage));
			return;
		case "ligo":
			vscode.window.showErrorMessage("LIGO reported error", largeError(formattedMessage));
			return;
		case "adapter-ligo":
			const versionIssues = response.body.error?.variables?.versionIssues
			vscode.window.showErrorMessage("Unexpected output from LIGO", largeError
				( isDefined(versionIssues)
					? versionIssues
					: "Some unexpected error when communicating with LIGO"
				, "Details: " + formattedMessage
				));
			return;
		case "adapter-plugin":
		case "adapter":
			// TODO [LIGO-892]: we need to provide details on how to reach us
			vscode.window.showErrorMessage("Internal error happened", largeError
				( "Please contact us."
				, "Details: " + formattedMessage
				));
			return;
		default:
			vscode.window.showErrorMessage("Some error", largeError(formattedMessage));
			return;
	}
}

// This class is copypasted from https://github.com/microsoft/vscode-debugadapter-node/blob/main/testSupport/src/protocolClient.ts
// because when it encounters an `ErrorResponse` it just throws `response.message` field
// and this is not convenient for us. We want to work with error responses too.
class ProtocolClient extends ee.EventEmitter {

	private static TWO_CRLF = '\r\n\r\n';

	private outputStream: stream.Writable;
	private sequence: number;
	private pendingRequests = new Map<number, (e: DebugProtocol.Response) => void>();
	private rawData = Buffer.alloc(0);
	private contentLength: number;

	constructor() {
		super();
		this.sequence = 1;
		this.contentLength = -1;
	}

	protected connect(readable: stream.Readable, writable: stream.Writable): void {

		this.outputStream = writable;

		readable.on('data', (data: Buffer) => {
			this.handleData(data);
		});
	}

	public send(command: 'initialize', args: DebugProtocol.InitializeRequestArguments): Promise<DebugProtocol.InitializeResponse>;
	public send(command: 'configurationDone', args: DebugProtocol.ConfigurationDoneArguments): Promise<DebugProtocol.ConfigurationDoneResponse>;
	public send(command: 'launch', args: DebugProtocol.LaunchRequestArguments): Promise<DebugProtocol.LaunchResponse>;
	public send(command: 'attach', args: DebugProtocol.AttachRequestArguments): Promise<DebugProtocol.AttachResponse>;
	public send(command: 'restart', args: DebugProtocol.RestartArguments): Promise<DebugProtocol.RestartResponse>;
	public send(command: 'disconnect', args: DebugProtocol.DisconnectArguments): Promise<DebugProtocol.DisconnectResponse>;
	public send(command: 'setBreakpoints', args: DebugProtocol.SetBreakpointsArguments): Promise<DebugProtocol.SetBreakpointsResponse>;
	public send(command: 'setFunctionBreakpoints', args: DebugProtocol.SetFunctionBreakpointsArguments): Promise<DebugProtocol.SetFunctionBreakpointsResponse>;
	public send(command: 'setExceptionBreakpoints', args: DebugProtocol.SetExceptionBreakpointsArguments): Promise<DebugProtocol.SetExceptionBreakpointsResponse>;
	public send(command: 'dataBreakpointInfo', args: DebugProtocol.DataBreakpointInfoArguments): Promise<DebugProtocol.DataBreakpointInfoResponse>;
	public send(command: 'setDataBreakpoints', args: DebugProtocol.SetDataBreakpointsArguments): Promise<DebugProtocol.SetDataBreakpointsResponse>;
	public send(command: 'continue', args: DebugProtocol.ContinueArguments): Promise<DebugProtocol.ContinueResponse>;
	public send(command: 'next', args: DebugProtocol.NextArguments): Promise<DebugProtocol.NextResponse>;
	public send(command: 'stepIn', args: DebugProtocol.StepInArguments): Promise<DebugProtocol.StepInResponse>;
	public send(command: 'stepOut', args: DebugProtocol.StepOutArguments): Promise<DebugProtocol.StepOutResponse>;
	public send(command: 'stepBack', args: DebugProtocol.StepBackArguments): Promise<DebugProtocol.StepBackResponse>;
	public send(command: 'reverseContinue', args: DebugProtocol.ReverseContinueArguments): Promise<DebugProtocol.ReverseContinueResponse>;
	public send(command: 'restartFrame', args: DebugProtocol.RestartFrameArguments): Promise<DebugProtocol.RestartFrameResponse>;
	public send(command: 'goto', args: DebugProtocol.GotoArguments): Promise<DebugProtocol.GotoResponse>;
	public send(command: 'pause', args: DebugProtocol.PauseArguments): Promise<DebugProtocol.PauseResponse>;
	public send(command: 'stackTrace', args: DebugProtocol.StackTraceArguments): Promise<DebugProtocol.StackTraceResponse>;
	public send(command: 'scopes', args: DebugProtocol.ScopesArguments): Promise<DebugProtocol.ScopesResponse>;
	public send(command: 'variables', args: DebugProtocol.VariablesArguments): Promise<DebugProtocol.VariablesResponse>;
	public send(command: 'setVariable', args: DebugProtocol.SetVariableArguments): Promise<DebugProtocol.SetVariableResponse>;
	public send(command: 'source', args: DebugProtocol.SourceArguments): Promise<DebugProtocol.SourceResponse>;
	public send(command: 'threads'): Promise<DebugProtocol.ThreadsResponse>;
	public send(command: 'modules'): Promise<DebugProtocol.ModulesResponse>;
	public send(command: 'evaluate', args: DebugProtocol.EvaluateArguments): Promise<DebugProtocol.EvaluateResponse>;
	public send(command: 'stepInTargets', args: DebugProtocol.StepInTargetsArguments): Promise<DebugProtocol.StepInTargetsResponse>;
	public send(command: 'gotoTargets', args: DebugProtocol.GotoTargetsArguments): Promise<DebugProtocol.GotoTargetsResponse>;
	public send(command: 'completions', args: DebugProtocol.CompletionsArguments): Promise<DebugProtocol.CompletionsResponse>;
	public send(command: 'exceptionInfo', args: DebugProtocol.ExceptionInfoArguments): Promise<DebugProtocol.ExceptionInfoResponse>;
	public send(command: string, args?: any): Promise<DebugProtocol.Response>;

	public send(command: string, args?: any): Promise<DebugProtocol.Response> {

		return new Promise((completeDispatch, errorDispatch) => {
			this.doSend(command, args, (result: DebugProtocol.Response) => {
				if (result.success) {
					completeDispatch(result);
				} else {
					// This should be safe because of this comment from docs:
					// On error (whenever 'success' is false), the body can provide more details.
					processErrorResponse(result as DebugProtocol.ErrorResponse);
					errorDispatch();
				}
			});
		});
	}

	private doSend(command: string, args: any, clb: (result: DebugProtocol.Response) => void): void {

		const request: DebugProtocol.Request = {
			type: 'request',
			seq: this.sequence++,
			command: command
		};
		if (args && Object.keys(args).length > 0) {
			request.arguments = args;
		}

		// store callback for this request
		this.pendingRequests.set(request.seq, clb);

		const json = JSON.stringify(request);
		this.outputStream.write(`Content-Length: ${Buffer.byteLength(json, 'utf8')}\r\n\r\n${json}`, 'utf8');
	}

	private handleData(data: Buffer): void {

		this.rawData = Buffer.concat([this.rawData, data]);

		while (true) {
			if (this.contentLength >= 0) {
				if (this.rawData.length >= this.contentLength) {
					const message = this.rawData.toString('utf8', 0, this.contentLength);
					this.rawData = this.rawData.slice(this.contentLength);
					this.contentLength = -1;
					if (message.length > 0) {
						this.dispatch(message);
					}
					continue;	// there may be more complete messages to process
				}
			} else {
				const idx = this.rawData.indexOf(ProtocolClient.TWO_CRLF);
				if (idx !== -1) {
					const header = this.rawData.toString('utf8', 0, idx);
					const lines = header.split('\r\n');
					for (let i = 0; i < lines.length; i++) {
						const pair = lines[i].split(/: +/);
						if (pair[0] === 'Content-Length') {
							this.contentLength = +pair[1];
						}
					}
					this.rawData = this.rawData.slice(idx + ProtocolClient.TWO_CRLF.length);
					continue;
				}
			}
			break;
		}
	}

	private dispatch(body: string): void {

		const rawData = JSON.parse(body);

		if (typeof rawData.event !== 'undefined') {
			const event = <DebugProtocol.Event>rawData;
			this.emit(event.event, event);
		} else {
			const response = <DebugProtocol.Response>rawData;
			const clb = this.pendingRequests.get(response.request_seq);
			if (clb) {
				this.pendingRequests.delete(response.request_seq);
				clb(response);
			}
		}
	}
}

export default class LigoProtocolClient extends ProtocolClient {
	socket: Net.Socket
	constructor(pipeName: string) {
		super();
		this.socket = Net.createConnection({ path: pipeName }, () => {
			this.connect(this.socket, this.socket);
		});
	}

	sendMsg(command: 'initializeLogger', args: InitializeLoggerArguments): Promise<InitializeLoggerResponse>
	sendMsg(command: 'setLigoBinaryPath', args: SetLigoBinaryPathArguments): Promise<SetLigoBinaryPathResponse>
	sendMsg(command: 'setProgramPath', args: SetProgramPathArguments): Promise<SetProgramPathResponse>
	sendMsg(command: 'validateEntrypoint', args: ValidateEntrypointArguments): Promise<ValidateEntrypointResponse>
	sendMsg(command: 'getContractMetadata', args: GetContractMetadataArguments): Promise<GetContractMetadataResponse>
	sendMsg(command: 'validateValue', args: ValidateValueArguments): Promise<ValidateValueResponse>
	sendMsg(command: LigoSpecificRequest, args: any): Promise<DebugProtocol.Response> {
		return this.send(command, args)
	}
}
