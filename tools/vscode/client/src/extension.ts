/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import { join } from 'path';
import { platform } from 'process';

import { LspExtension } from './lsp/LspExtension';
import { LigoContext } from './common/LigoContext';
import { LigoProtocolClient } from './common/LigoProtocolClient';

import { DebuggerExtension } from './debugger/DebuggerExtension';
import LigoServer from './debugger/LigoServer';
import { getCurrentWorkspacePath } from './debugger/base';
import { trackLigoPathChanges } from './common/config';

export async function activate(context: vscode.ExtensionContext) {
  const ligoContext = new LigoContext(context);

  const adapterPath = join(context.extensionPath, 'bin', `ligo-debugger${platform === 'win32' ? '.exe' : ''}`);
  const server = new LigoServer(getCurrentWorkspacePath()?.fsPath, adapterPath, []);
  const client = new LigoProtocolClient(server.address());

  context.subscriptions.push(
    trackLigoPathChanges()
  )

  context.subscriptions.push(
    new LspExtension(ligoContext, client),
    new DebuggerExtension(ligoContext, server, client),
  )
}

