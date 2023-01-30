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

import { extensions } from './common'
import { changeLastContractPath } from './commands/common';

let client: LanguageClient;
let ligoOptionButton: vscode.StatusBarItem;
let deployOptionButton: vscode.StatusBarItem;

// Hides compilation button in case current active text editor is not .(m/re)ligo file
// If currently active text window is not an opened file (terminal, explorer, etc.)
// button will remain in it's previous state
function updateLigoButton(button: vscode.StatusBarItem) {
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
function initializeStatusBarButton(
  button: vscode.StatusBarItem,
  context: vscode.ExtensionContext,
  command: string,
  title: string,
  tooltip: string,
) {
  button.command = command;
  button.text = title;
  button.tooltip = tooltip;
  context.subscriptions.push(button);

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(
      () => updateLigoButton(button),
    ),
  );
  context.subscriptions.push(
    vscode.window.onDidChangeTextEditorSelection(
      () => updateLigoButton(button),
    ),
  );
  updateLigoButton(button);
}

export async function activate(context: vscode.ExtensionContext) {
  if (context.extensionMode === vscode.ExtensionMode.Production) {
    await updateLigo()
    await updateExtension(context)
  }

  const serverOptions: ServerOptions = {
    command: `${context.extensionPath}/bin/ligo-squirrel`,
    options: {
      cwd: `${context.extensionPath}`,
    },
  };

  // Initializes buttons, and command state
  initializeExtensionState();
  ligoOptionButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
  deployOptionButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
  initializeStatusBarButton(ligoOptionButton, context, 'ligo.chooseOption', 'LIGO Options', 'Display LIGO options');
  initializeStatusBarButton(deployOptionButton, context, 'tezos.chooseOption', 'Deploy LIGO', 'Deploy smart-contract');

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
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

  // Create the language client and start the client.
  client = new LanguageClient(
    'ligoLanguageServer',
    'LIGO Language Server',
    serverOptions,
    clientOptions,
  );

  // Register VSC-specific server commands
  registerCommands(client);

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
