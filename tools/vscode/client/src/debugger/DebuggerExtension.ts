import * as vscode from 'vscode'
import { join } from 'path'
import { platform } from 'process'

import LigoDebugAdapterServerDescriptorFactory from './LigoDebugAdapterDescriptorFactory'
import LigoDebugConfigurationProvider from './LigoDebugConfigurationProvider'
import { LigoProtocolClient, GranularityFillingTrackerFactory } from './LigoProtocolClient'
import LigoServer from './LigoServer'
import { DebugSteppingGranularityStatus, createConfigSnippet } from './ui'
import { isDefined, getCurrentWorkspacePath } from './base'
import { LigoDebugContext } from './LigoDebugContext'
import { LigoDebugAdapterTrackerFactory } from './LigoDebugAdapterTrackerFactory'
import { LigoExtension } from '../LigoExtension'

export class DebuggerExtension extends LigoExtension {
  private server: LigoServer;
  private client: LigoProtocolClient;
  private stepStatus = new DebugSteppingGranularityStatus(async _granularity => { });

  public activate(context: vscode.ExtensionContext): void {
    const adapterPath = join(context.extensionPath, 'bin', `ligo-debugger${platform === 'win32' ? '.exe' : ''}`)

    const documentProvider = new class implements vscode.TextDocumentContentProvider {
      onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>();
      onDidChange = this.onDidChangeEmitter.event;

      provideTextDocumentContent(uri: vscode.Uri): string {
        return uri.query;
      }
    }

    context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider('ligo', documentProvider));

    this.server = new LigoServer(getCurrentWorkspacePath()?.fsPath, adapterPath, [])
    this.client = new LigoProtocolClient(this.server.address())

    context.subscriptions.push(
      this.stepStatus,
      vscode.debug.onDidStartDebugSession(() => this.stepStatus.show()),
      vscode.debug.onDidTerminateDebugSession(() => this.stepStatus.hide())
    )
    const trackerFactory = new GranularityFillingTrackerFactory(() => this.stepStatus.status)
    context.subscriptions.push(vscode.debug.registerDebugAdapterTrackerFactory('ligo', trackerFactory))

    const ligoDebugContext = new LigoDebugContext(context);

    const askOnStartCommandChanged = ligoDebugContext.globalState.askOnStartCommandChanged();
    if (!isDefined(askOnStartCommandChanged.value) || !askOnStartCommandChanged.value) {
      vscode.window.showWarningMessage(
        `Note that syntax for commands in launch.json have changed.
        Now commands are wrapped into (*@ and @*) instead of { and }.
        For more information see README.md`, "OK"
      ).then(result => {
        if (isDefined(result)) {
          askOnStartCommandChanged.value = true;
        }
      });
    }

    const provider = new LigoDebugConfigurationProvider(this.client, ligoDebugContext);
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ligo', provider))

    const factory = new LigoDebugAdapterServerDescriptorFactory(this.server)
    context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ligo', factory))
    if ('dispose' in factory) {
      context.subscriptions.push(factory)
    }

    context.subscriptions.push(
      vscode.debug.registerDebugAdapterTrackerFactory(
        'ligo',
        new LigoDebugAdapterTrackerFactory(),
      )
    );

    vscode.commands.registerCommand(
      'extension.ligo-debugger.createLigoConfig',
      () => createConfigSnippet(ligoDebugContext)
    );
  }

  public deactivate(): void {
    this.server.dispose();
  }
}
