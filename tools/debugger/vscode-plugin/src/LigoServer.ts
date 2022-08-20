import * as cp from 'child_process'
import * as fs from 'fs'
import * as net from 'net'
import * as vscode from 'vscode'
import { randomBytes } from 'crypto'
import { tmpdir } from 'os'
import { join } from 'path'
import { platform } from 'process'

// Server to forward requests from the debugger adapter to the process stdin and
// stdout of the process to the client.
export default class LigoServer implements vscode.Disposable {
	server: net.Server
	adapterProcess: cp.ChildProcess

	public constructor(command: string, args: ReadonlyArray<string>) {
		if (!fs.existsSync(command)) {
			this.showError("Couldn't find debugger adapter executable")
		}

		let adapterProcess = cp.spawn(command, args)
		if (!adapterProcess || !adapterProcess.pid) {
			this.showError("Couldn't run debugger adapter")
		}

		// start listening on a random named pipe path
		const pipeName = randomBytes(10).toString('utf8')
		const pipePath = platform === "win32" ? join('\\\\.\\pipe\\', pipeName) : join(tmpdir(), pipeName)

		this.server = net.createServer(socket => {
			const requestsListener = (bytes: Buffer) => {
				// Just forward client request to the adapter
				adapterProcess.stdin.write(bytes)
			}

			const responsesListener = (bytes: Buffer) => {
				// Just forward the adapter response to the client
				if (socket.writable) {
					socket.write(bytes)
				}
			}

			socket.on('data', requestsListener)
			adapterProcess.stdout.on('data', responsesListener)

			socket.on('end', () => {
				// gracefully remove all listeners, to avoid spamming to the adapter
				socket.removeListener('data', requestsListener)
				adapterProcess.stdout.removeListener('data', responsesListener)
			})
		}).listen(pipePath)

		if (!this.server.listening)
			this.showError("Adapter server couldn't start listening on pipe " + pipePath + ". Try again")
	}


	address() {
		return this.server.address() as string
	}

	port() {
		return (this.server.address() as net.AddressInfo).port
	}

	dispose() {
		this.adapterProcess.kill()
		this.server.close()
	}

	private showError(msg: string) {
		vscode.window.showErrorMessage(msg).
			then(_ => undefined)
		throw new Error(msg)
	}
}
