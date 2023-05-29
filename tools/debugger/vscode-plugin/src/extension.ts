import * as vscode from 'vscode'
import { join } from 'path'
import { platform } from 'process'

import LigoDebugAdapterServerDescriptorFactory from './LigoDebugAdapterDescriptorFactory'
import LigoDebugConfigurationProvider from './LigoDebugConfigurationProvider'
import { LigoProtocolClient, GranularityFillingTrackerFactory } from './LigoProtocolClient'
import LigoServer from './LigoServer'
import { createRememberingQuickPick, getEntrypoint, getParameterOrStorage, DebugSteppingGranularityStatus } from './ui'
import { Ref, Maybe, getBinaryPath, getCommand, isDefined, InputValueType, InputValidationResult} from './base'
import { LigoDebugContext } from './LigoDebugContext'
import { LigoDebugAdapterTrackerFactory } from './LigoDebugAdapterTrackerFactory'

let server: LigoServer
let client: LigoProtocolClient

const stepStatus = new DebugSteppingGranularityStatus(async _granularity => {})

export function activate(context: vscode.ExtensionContext) {
	const adapterPath = join(context.extensionPath, 'bin', `ligo-debugger${platform === 'win32' ? '.exe' : ''}`)

	server = new LigoServer(adapterPath, [])
	client = new LigoProtocolClient(server.address())

	context.subscriptions.push(
		stepStatus,
		vscode.debug.onDidStartDebugSession(() => stepStatus.show()),
		vscode.debug.onDidTerminateDebugSession(() => stepStatus.hide())
	)
	const trackerFactory = new GranularityFillingTrackerFactory(() => stepStatus.status)
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

	const provider = new LigoDebugConfigurationProvider(client, ligoDebugContext);
	context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ligo', provider))

	const factory = new LigoDebugAdapterServerDescriptorFactory(server)
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
}

export function deactivate() {
	server.dispose()
}
