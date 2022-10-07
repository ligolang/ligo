import * as vscode from 'vscode'
import { join } from 'path'
import { platform } from 'process'

import { ValidateValueCategory } from './messages'
import LigoDebugAdapterServerDescriptorFactory from './LigoDebugAdapterDescriptorFactory'
import LigoDebugConfigurationProvider, { AfterConfigResolvedInfo } from './LigoDebugConfigurationProvider'
import LigoProtocolClient from './LigoProtocolClient'
import { createRememberingQuickPick, getEntrypoint, getParameterOrStorage, InputValueType } from './ui'
import LigoServer from './LigoServer'
import { Ref, DebuggedContractSession, Maybe, getBinaryPath, getCommand, isDefined } from './base'
import { LigoDebugContext } from './LigoDebugContext'

let server: LigoServer
let client: LigoProtocolClient

// This variable is used to provide additional information
// about currently launching contract.
// 'createRememberingQuickPick' and 'ConfigurationProvider' write here,
// 'createRememberingInputBox' reads.
const debuggedContractSession: Ref<DebuggedContractSession> = { ref: {} }

export function activate(context: vscode.ExtensionContext) {
	const adapterPath = join(context.extensionPath, 'bin', `ligo-debugger${platform === 'win32' ? '.exe' : ''}`)

	server = new LigoServer(adapterPath, [])
	client = new LigoProtocolClient(server.address())

	const getContractMetadata = async (entrypoint: string): Promise<void> => {
		debuggedContractSession.ref.contractMetadata =
			(await client.sendMsg('getContractMetadata', { entrypoint }))
				.contractMetadata
	}

	const provider = new LigoDebugConfigurationProvider(
		async (info: AfterConfigResolvedInfo): Promise<string> => {
			await client.sendMsg('initializeLogger', { file: info.file, logDir: info.logDir })

			const config = vscode.workspace.getConfiguration();
			const binaryPath = getBinaryPath({ name: 'ligo', path: 'ligoDebugger.ligoBinaryPath' }, config);
			await client.sendMsg('setLigoBinaryPath', { binaryPath });

			debuggedContractSession.ref.entrypoints =
				(await client.sendMsg('setProgramPath', { program: info.file })).entrypoints.reverse() as [string];

			const entrypointCommand = getCommand(info.entrypoint);
			if (entrypointCommand === 'AskForEntrypoint') {
				const entrypoint: string = await vscode.commands.executeCommand('extension.ligo-debugger.requestEntrypoint');
				if (!isDefined(entrypoint)) {
					// If user decided to close entrypoint quickpick
					// then we want to stop debugging session immediately.
					// We can do this by throwing something (tried to throw `Error`
					// but I see annoying error message in bottom right corner).
					//
					// Note: 1000 - 7 is just a random value. We're throwing it
					// in order not to trigger `vscode` to show error message.
					throw 1000 - 7;
				} else {
					return entrypoint;
				}
			} else {
				await getContractMetadata(info.entrypoint);
				return info.entrypoint;
			}
		});
	context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ligo', provider))

	const factory = new LigoDebugAdapterServerDescriptorFactory(server)
	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ligo', factory))
	if ('dispose' in factory) {
		context.subscriptions.push(factory)
	}

	const validateInput = (category: ValidateValueCategory, valueType: InputValueType) => async (value: string): Promise<Maybe<string>> => {
		if (client) {
			const pickedMichelsonEntrypoint = debuggedContractSession.ref.pickedMichelsonEntrypoint
			return (await client.sendMsg('validateValue', { value, category, valueType, pickedMichelsonEntrypoint })).message
		}
		return undefined
	}

	const validateEntrypoint = async (entrypoint: string): Promise<Maybe<string>> => {
		if (client) {
			return (await client.sendMsg('validateEntrypoint', { entrypoint })).message;
		}
		return undefined;
	}

	const ourContext = new LigoDebugContext(context);

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.ligo-debugger.requestEntrypoint',
			getEntrypoint(
				ourContext,
				validateEntrypoint,
				getContractMetadata,
				debuggedContractSession)));

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.ligo-debugger.requestMichelsonEntrypoint',
			createRememberingQuickPick(
				debuggedContractSession,
				"Please pick a Michelson entrypoint to run")));

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.ligo-debugger.requestParameterValue',
			getParameterOrStorage(
				ourContext,
				validateInput,
				"parameter",
				"Input the contract parameter",
				"Parameter value",
				debuggedContractSession)));

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.ligo-debugger.requestStorageValue',
			getParameterOrStorage(
				ourContext,
				validateInput,
				"storage",
				"Input the contract storage",
				"Storage value",
				debuggedContractSession)));

}

export function deactivate() {
	server.dispose()
}
