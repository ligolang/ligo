import { DebugProtocol } from '@vscode/debugprotocol/lib/debugProtocol';
import * as vscode from 'vscode';
import { isDefined, Maybe } from '../common/base';
import { interruptExecution } from './base';
import { processErrorResponse } from '../common/LigoProtocolClient';

class LigoDebugAdapterTracker implements vscode.DebugAdapterTracker {
  private readonly session: vscode.DebugSession;

  public constructor(session: vscode.DebugSession) {
    this.session = session;
  }

  private static castMessage(message: any): message is DebugProtocol.Response {
    // According to DAP spec.
    return 'seq' in message && 'type' in message && message.type == 'response';
  }

  private static shouldInterruptDebuggingSession(errResponse: DebugProtocol.ErrorResponse): boolean {
    const shouldInterruptDebuggingSession: Maybe<string> =
      errResponse.body.error?.variables?.shouldInterruptDebuggingSession;

    if (isDefined(shouldInterruptDebuggingSession) && shouldInterruptDebuggingSession === 'true') {
      return true;
    } else {
      return false;
    }
  }

  public onDidSendMessage(message: any): void {
    if (LigoDebugAdapterTracker.castMessage(message) && !message.success) {
      const errResponse = message as DebugProtocol.ErrorResponse;
      processErrorResponse(errResponse);
      if (LigoDebugAdapterTracker.shouldInterruptDebuggingSession(errResponse)) {
        interruptExecution();
      }
    }
  }
}

export class LigoDebugAdapterTrackerFactory implements vscode.DebugAdapterTrackerFactory {
  public createDebugAdapterTracker(session: vscode.DebugSession): vscode.ProviderResult<vscode.DebugAdapterTracker> {
    return new LigoDebugAdapterTracker(session);
  }
}
