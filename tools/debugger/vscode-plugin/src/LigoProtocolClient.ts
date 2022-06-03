import { ProtocolClient } from "@vscode/debugadapter-testsupport/lib/protocolClient";
import * as Net from 'net';
import {
  InitializeLoggerArguments,
  InitializeLoggerResponse
} from "./Messages";
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
  sendMsg(command: 'initializeLogger', args: any): Promise<DebugProtocol.Response> {
    return this.send(command, args)
  }
}
