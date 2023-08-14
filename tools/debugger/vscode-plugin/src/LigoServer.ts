import * as cp from 'child_process'
import * as fs from 'fs'
import * as net from 'net'
import * as vscode from 'vscode'
import { randomBytes } from 'crypto'
import { tmpdir } from 'os'
import { join } from 'path'
import { platform } from 'process'
import { Maybe } from './base'

// Server to forward requests from the debugger adapter to the process stdin and
// stdout of the process to the client.
export default class LigoServer implements vscode.Disposable {
  server: net.Server
  adapterProcess: cp.ChildProcess
  adapterIsDead: boolean = false

  private createAdapterProcess(cwd: Maybe<string>, command: string, args: ReadonlyArray<string>) {
    this.adapterProcess = cp.spawn(command, args, { cwd })
    if (!this.adapterProcess || !this.adapterProcess.pid) {
      this.showError("Couldn't run debugger adapter")
    }
  }

  public constructor(cwd: Maybe<string>, command: string, args: ReadonlyArray<string>) {
    if (!fs.existsSync(command)) {
      this.showError("Couldn't find debugger adapter executable")
    }

    this.createAdapterProcess(cwd, command, args)

    // start listening on a random named pipe path
    const pipeName = "ligo-debugger-pipe-" + randomBytes(10).toString('hex')
    const pipePath = platform === "win32" ? join('\\\\.\\pipe\\', pipeName) : join(tmpdir(), pipeName)

    this.server = net.createServer(socket => {
      this.adapterProcess.on('exit', (_) => this.adapterIsDead = true)

      const responsesListener = (bytes: Buffer) => {
        // Just forward the adapter response to the client
        if (socket.writable) {
          socket.write(bytes)
        }
      }

      const requestsListener = (bytes: Buffer) => {
        // If adapter got killed then we need to reanimate this process
        if (this.adapterIsDead) {
          this.createAdapterProcess(cwd, command, args)
          this.adapterProcess.stdout.on('data', responsesListener)
          this.adapterIsDead = false
        }

        // Just forward client request to the adapter
        this.adapterProcess.stdin.write(bytes)
      }

      socket.on('data', requestsListener)
      this.adapterProcess.stdout.on('data', responsesListener)

      socket.on('end', () => {
        // gracefully remove all listeners, to avoid spamming to the adapter
        socket.removeListener('data', requestsListener)
        this.adapterProcess.stdout.removeListener('data', responsesListener)
      })
    }).listen(pipePath)

    if (!this.server.listening) {
      this.showError("Adapter server couldn't start listening on pipe " + pipePath + ". Try again")
    }
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
