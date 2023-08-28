/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';

import { LspExtension } from './lsp/LspExtension';
import { DebuggerExtension } from './debugger/DebuggerExtension';

const lspExtension = new LspExtension();
const debuggerExtension = new DebuggerExtension();

export async function activate(context: vscode.ExtensionContext) {
  lspExtension.activate(context);
  debuggerExtension.activate(context);
}

export function deactivate() {
  lspExtension.deactivate();
  debuggerExtension.deactivate();
}
