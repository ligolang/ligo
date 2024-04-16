import * as cp from 'child_process'
import * as fs from 'fs'
import * as net from 'net'
import * as vscode from 'vscode'
import { randomBytes } from 'crypto'
import { tmpdir } from 'os'
import { join } from 'path'
import { platform } from 'process'
import { Maybe } from '../common/base'

/**
 * Server to forward requests from the debugger adapter to the process stdin and
 * stdout of the process to the client.
 */
export default class LigoServer implements vscode.Disposable {
  server: net.Server
  adapterProcess: cp.ChildProcessWithoutNullStreams
  adapterIsDead: boolean = false

  private static createAdapterProcess(cwd: Maybe<string>, command: string, args: ReadonlyArray<string>): cp.ChildProcessWithoutNullStreams {
    const adapterProcess = cp.spawn(command, args, { cwd })
    if (!adapterProcess || !adapterProcess.pid) {
      this.showError("Couldn't run debugger adapter")
    }
    return adapterProcess
  }

  /**
   * Creates a named pipe in order to forward the debug adapter response to the
   * client. Properly handle the cases when the pipe receives data, or exits,
   * either with success or failure.
   */
  public constructor(cwd: Maybe<string>, command: string, args: ReadonlyArray<string>) {
    if (!fs.existsSync(command)) {
      LigoServer.showError("Couldn't find debugger adapter executable")
    }

    this.adapterProcess = LigoServer.createAdapterProcess(cwd, command, args)

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
          this.adapterProcess = LigoServer.createAdapterProcess(cwd, command, args)
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
      LigoServer.showError(`Adapter server couldn't start listening on pipe ${pipePath}. Try again`)
    }
  }

  /** Returns the address of this server. */
  address() {
    return this.server.address() as string
  }

  /** Returns the port number of this server. */
  port() {
    return (this.server.address() as net.AddressInfo).port
  }

  /** Releases resources acquired by this server. */
  dispose() {
    this.adapterProcess.kill()
    this.server.close()
  }

  private static showError(msg: string) {
    vscode.window.showErrorMessage(msg).
      then(_ => undefined)
    throw new Error(msg)
  }
}
