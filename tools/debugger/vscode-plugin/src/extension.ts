import * as vscode from 'vscode'
import { join } from 'path'
import { platform } from 'process'

import LigoDebugAdapterServerDescriptorFactory from './LigoDebugAdapterDescriptorFactory'
import LigoDebugConfigurationProvider from './LigoDebugConfigurationProvider'
import LigoProtocolClient from './LigoProtocolClient'
import LigoServer from './LigoServer'

let server: LigoServer
let client: LigoProtocolClient

export function activate(context: vscode.ExtensionContext) {
  const adapterPath = join(context.extensionPath, 'bin', `ligo-debugger${platform === 'win32' ? '.exe' : ''}`)

  server = new LigoServer(adapterPath, [])
  client = new LigoProtocolClient(server.address())

  const provider = new LigoDebugConfigurationProvider(
    async (file: string, logDir: string): Promise<void> => {
      await client.sendMsg('initializeLogger', {file, logDir})
  });
  context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ligo', provider))

  const factory = new LigoDebugAdapterServerDescriptorFactory(server)
  context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ligo', factory))
  if ('dispose' in factory) {
    context.subscriptions.push(factory)
  }
}

export function deactivate() {
  server.dispose()
}
