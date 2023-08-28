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
import { initializeExtensionState } from './ui'
import updateExtension from './updateExtension'
import updateLigo from './updateLigo'
import { BinaryNotFoundException } from './exceptions'
import { LigoExtension } from "../LigoExtension";

import { extensions } from './common'
import { changeLastContractPath, getBinaryPath, ligoBinaryInfo } from './commands/common';

export class LspExtension extends LigoExtension {
  private client: LanguageClient;
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


  public activate(context: vscode.ExtensionContext): void {
    const config = vscode.workspace.getConfiguration()
    let ligoPath: string
    try {
      ligoPath = getBinaryPath(ligoBinaryInfo, config)
    } catch (err) {
      if (err instanceof BinaryNotFoundException) {
        ligoPath = undefined
      } else {
        throw err
      }
    }

    const serverOptions: ServerOptions = {
      command: ligoPath,
      args: ["lsp"],
      options: {
        cwd: context.extensionPath,
      },
    };

    // Initializes buttons, and command state
    initializeExtensionState();
    this.ligoOptionButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
    this.deployOptionButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
    this.initializeStatusBarButton(this.ligoOptionButton, context, 'ligo.chooseOption', 'LIGO Options', 'Display LIGO options');
    this.initializeStatusBarButton(this.deployOptionButton, context, 'tezos.chooseOption', 'Deploy LIGO', 'Deploy smart-contract');

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
      // Register the server for plain text documents
      documentSelector: [
        { scheme: 'file', language: 'ligo' },
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

    // Create the language client and start the client.
    this.client = new LanguageClient(
      'ligoLanguageServer',
      'LIGO Language Server',
      serverOptions,
      clientOptions
    );

    // Check for LIGO and ligo-vscode updates
    if (context.extensionMode === vscode.ExtensionMode.Production) {
      updateLigo(this.client)
      updateExtension(context)
    }

    // Register VSC-specific server commands
    registerCommands(this.client);

    // Start the client. This will also launch the server
    if (ligoPath) {
      this.client.start()
    }
  }

  public deactivate(): Thenable<void> | undefined {
    return this.client?.stop();
  }
}
