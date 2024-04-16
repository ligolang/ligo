import * as vscode from 'vscode'

import LigoDebugAdapterServerDescriptorFactory from './LigoDebugAdapterDescriptorFactory'
import LigoDebugConfigurationProvider from './LigoDebugConfigurationProvider'
import { LigoProtocolClient, GranularityFillingTrackerFactory } from '../common/LigoProtocolClient'
import LigoServer from './LigoServer'
import { DebugSteppingGranularityStatus, createConfigSnippet } from './ui'
import { LigoContext } from '../common/LigoContext'
import { LigoDebugAdapterTrackerFactory } from './LigoDebugAdapterTrackerFactory'
import { isDefined } from '../common/base'

/**
 * The main class for the debugger. An instance of this object holds all the
 * necessary state for launching and running the debugger.
 */
export class DebuggerExtension implements vscode.Disposable {
  private server: LigoServer;
  private client: LigoProtocolClient;
  private stepStatus = new DebugSteppingGranularityStatus(async _granularity => { });

  /**
   * Registers all features needed for the debugger's functioning, initializes
   * all states, and registers commands.
   */
  public constructor(context: LigoContext, server: LigoServer, client: LigoProtocolClient) {
    const documentProvider = new class implements vscode.TextDocumentContentProvider {
      onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>();
      onDidChange = this.onDidChangeEmitter.event;

      provideTextDocumentContent(uri: vscode.Uri): string {
        return uri.query;
      }
    }

    context.context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider('ligo', documentProvider));

    this.server = server;
    this.client = client;

    context.context.subscriptions.push(
      this.stepStatus,
      vscode.debug.onDidStartDebugSession(() => this.stepStatus.show()),
      vscode.debug.onDidTerminateDebugSession(() => this.stepStatus.hide())
    )
    const trackerFactory = new GranularityFillingTrackerFactory(() => this.stepStatus.status)
    context.context.subscriptions.push(vscode.debug.registerDebugAdapterTrackerFactory('ligo', trackerFactory))

    const provider = new LigoDebugConfigurationProvider(this.client, context);
    context.context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ligo', provider))

    const factory = new LigoDebugAdapterServerDescriptorFactory(this.server)
    context.context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ligo', factory))
    if ('dispose' in factory) {
      context.context.subscriptions.push(factory)
    }

    context.context.subscriptions.push(
      vscode.debug.registerDebugAdapterTrackerFactory(
        'ligo',
        new LigoDebugAdapterTrackerFactory(),
      )
    );

    vscode.commands.registerCommand(
      'extension.ligo-debugger.createLigoConfig',
      () => createConfigSnippet(context)
    );
  }

  /** Releases resources acquired by the debugger. */
  public dispose(): void {
    this.stepStatus.dispose();
  }
}
