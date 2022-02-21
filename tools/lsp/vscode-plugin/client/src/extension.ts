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

let client: LanguageClient;
let compileButton: vscode.StatusBarItem;


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

  const compileCommandId = 'ligo.compileContract';
  compileButton = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 0);
  compileButton.command = compileCommandId;
  compileButton.text = "Compile";
  context.subscriptions.push(compileButton);

  context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(updateCompileButton));
  context.subscriptions.push(vscode.window.onDidChangeTextEditorSelection(updateCompileButton));
  updateCompileButton();

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [
      { scheme: 'file', language: 'ligo' },
      { scheme: 'file', language: 'mligo' },
      { scheme: 'file', language: 'religo' },
    ],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      // fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
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

function updateCompileButton() {
  const ext = extname(vscode.window.activeTextEditor.document.uri.fsPath);
  if (ext == '.ligo' || ext == '.mligo' || ext == '.religo')
    compileButton.show();
  else
    compileButton.hide();
}
