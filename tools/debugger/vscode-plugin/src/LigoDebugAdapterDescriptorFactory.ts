import * as vscode from 'vscode'

import LigoServer from './LigoServer'

export default class DebugAdapterServerDescriptorFactory
	implements vscode.DebugAdapterDescriptorFactory {
	readonly server: LigoServer

	constructor(server: LigoServer) {
		this.server = server
	}

	createDebugAdapterDescriptor(
		_session: vscode.DebugSession,
		_executable: vscode.DebugAdapterExecutable | undefined
	): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
		return new vscode.DebugAdapterNamedPipeServer(this.server.address());
	}
}
