/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { ExtensionContext } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    let serverOptions: ServerOptions = {
       command: `${context.extensionPath}/bin/ligo-squirrel`
    };

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [
            { scheme: 'file', language: 'ligo' },
            { scheme: 'file', language: 'mligo' },
            { scheme: 'file', language: 'religo' }
        ],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
	    // fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'ligoLanguageServer',
        'LIGO Language Server',
        serverOptions,
        clientOptions
    );

    console.log('Starting the client...')
    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
