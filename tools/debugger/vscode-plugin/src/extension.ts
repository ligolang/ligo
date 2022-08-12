import * as vscode from 'vscode'
import { join } from 'path'
import { platform } from 'process'

import { ValidateValueCategory } from './messages'
import LigoDebugAdapterServerDescriptorFactory from './LigoDebugAdapterDescriptorFactory'
import LigoDebugConfigurationProvider, { AfterConfigResolvedInfo } from './LigoDebugConfigurationProvider'
import LigoProtocolClient from './LigoProtocolClient'
import { createRememberingQuickPick, getEntrypoint, getParameterOrStorage, ValueType } from './ui'
import LigoServer from './LigoServer'
import { Ref, DebuggedContractSession, Maybe, ContractMetadata } from './base'

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
		async (info: AfterConfigResolvedInfo): Promise<void> => {
			await client.sendMsg('initializeLogger', { file: info.file, logDir: info.logDir })
			debuggedContractSession.ref.entrypoints =
				(await client.sendMsg('setProgramPath', { program: info.file })).entrypoints
			if (info.entrypoint) {
				await getContractMetadata(info.entrypoint);
			}
		});
	context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ligo', provider))

	const factory = new LigoDebugAdapterServerDescriptorFactory(server)
	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ligo', factory))
	if ('dispose' in factory) {
		context.subscriptions.push(factory)
	}

	const validateInput = (category: ValidateValueCategory, valueType: ValueType) => async (value: string): Promise<Maybe<string>> => {
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

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.ligo-debugger.requestEntrypoint',
			getEntrypoint(
				context,
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
				context,
				validateInput,
				"parameter",
				"Please input the contract parameter",
				"Parameter value",
				debuggedContractSession)));

	context.subscriptions.push(
		vscode.commands.registerCommand('extension.ligo-debugger.requestStorageValue',
			getParameterOrStorage(context,
				validateInput,
				"storage",
				"Please input the contract storage",
				"Storage value",
				debuggedContractSession)));

}

export function deactivate() {
	server.dispose()
}
