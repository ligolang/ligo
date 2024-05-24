/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import { extname } from 'path';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

import { registerCommands } from './command'
import updateExtension from './updateExtension'
import updateLigo from './updateLigo'

import { extensions } from './common'
import { changeLastContractPath } from './commands/common';
import { LigoContext } from '../common/LigoContext';
import { LigoProtocolClient } from '../common/LigoProtocolClient';

import { getBinaryPath, ligoBinaryInfo } from '../common/config';

/**
 * The main class for the language server. An instance of this object holds all
 * the necessary state for launching and running the language server.
 */
export class LspExtension implements vscode.Disposable {
  private languageServerClient: LanguageClient;
  private semanticTokensClient: LanguageClient;
  private ligoOptionButton: vscode.StatusBarItem;
  private deployOptionButton: vscode.StatusBarItem;

  // Hides compilation button in case current active text editor is not .{m,js,p,}ligo file
  // If currently active text window is not an opened file (terminal, explorer, etc.)
  // button will remain in its previous state.
  private updateLigoButton(button: vscode.StatusBarItem): void {
    if (!vscode.window.activeTextEditor) {
      button.hide()
      return;
    }
    const path = vscode.window.activeTextEditor.document.uri.fsPath;
    const ext = extname(path);

    // Ignore vscode windows
    if (path.startsWith('extension')) {
      return;
    }

    if (extensions.includes(ext)) {
      changeLastContractPath(path)
      button.show();
    } else {
      button.hide();
    }
  }

  /* eslint-disable no-param-reassign */
  private initializeStatusBarButton(
    button: vscode.StatusBarItem,
    context: vscode.ExtensionContext,
    command: string,
    title: string,
    tooltip: string,
  ): void {
    button.command = command;
    button.text = title;
    button.tooltip = tooltip;
    context.subscriptions.push(button);

    context.subscriptions.push(
      vscode.window.onDidChangeActiveTextEditor(
        () => this.updateLigoButton(button),
      ),
    );
    context.subscriptions.push(
      vscode.window.onDidChangeTextEditorSelection(
        () => this.updateLigoButton(button),
      ),
    );
    this.updateLigoButton(button);
  }

  /**
   * Registers all features needed for the language server's functioning,
   * initializes all states, creates status bars buttons, and registers
   * commands. This will launch two language servers: LIGO Language Server, and
   * LIGO Semantic Tokens.
   */
  public constructor(context: LigoContext, protocolClient: LigoProtocolClient) {
    let ligoPath: string = getBinaryPath(ligoBinaryInfo)

    const languageServerServerOptions: ServerOptions = {
      command: ligoPath,
      args: ["lsp", "no-semantic-tokens"],
      options: {
        cwd: context.context.extensionPath,
      },
    };

    const semanticTokensServerOptions: ServerOptions = {
      command: ligoPath,
      args: ["lsp", "only-semantic-tokens"],
      options: {
        cwd: context.context.extensionPath,
      },
    };

    // Initializes buttons
    this.ligoOptionButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
    this.deployOptionButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
    this.initializeStatusBarButton(this.ligoOptionButton, context.context, 'ligo.chooseOption', 'LIGO Options', 'Display LIGO options');
    this.initializeStatusBarButton(this.deployOptionButton, context.context, 'tezos.chooseOption', 'Deploy LIGO', 'Deploy smart-contract');

    // Options to control the language client
    const languageServerClientOptions: LanguageClientOptions = {
      // Register the server for plain text documents
      documentSelector: [
        { scheme: 'file', language: 'mligo' },
        { scheme: 'file', language: 'jsligo' },
      ],
      synchronize: {
        // Notify the server about file changes to '.clientrc files contained in the workspace
        // fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
      },
      initializationOptions: {
        ligoLanguageServer: vscode.workspace.getConfiguration('ligoLanguageServer'),
      },
    };

    const semanticTokensClientOptions: LanguageClientOptions = languageServerClientOptions;

    // Register the server for plain text documents
    this.semanticTokensClient = new LanguageClient(
      'ligoSemanticTokens',
      'LIGO Semantic Tokens',
      semanticTokensServerOptions,
      semanticTokensClientOptions
    );

    // Create the language client and start the client.
    this.languageServerClient = new LanguageClient(
      'ligoLanguageServer',
      'LIGO Language Server',
      languageServerServerOptions,
      languageServerClientOptions
    );

    // Check for LIGO and ligo-vscode updates
    if (context.context.extensionMode === vscode.ExtensionMode.Production) {
      // TODO: handle semanticTokensClient here
      updateLigo(this.languageServerClient)
      updateExtension(context.context)
    }

    // Register VSC-specific server commands
    registerCommands(
      context,
      this.languageServerClient,
      this.semanticTokensClient,
      protocolClient,
    );

    // Start the client. This will also launch the server
    this.semanticTokensClient.start()
    this.languageServerClient.start()
  }

  /** Releases resources acquired by the language server. */
  public dispose(): Thenable<void> | undefined {
    this.semanticTokensClient?.stop();
    this.languageServerClient?.stop();
    return;
  }
}
