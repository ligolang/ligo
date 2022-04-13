const { IpcChannel } = require('@obsidians/ipc')
const TerminalChannel = require('./TerminalChannel')

class TerminalChannelManager extends IpcChannel {
  constructor() {
    super('terminal-creator')
    this.terminals = {}
  }

  create (logId, cwd) {
    if (this.terminals[logId]) {
      this.terminals[logId].pty.cwd = cwd
      return
    }
    const handler = new TerminalChannel(logId, cwd)
    this.terminals[logId] = handler
  }

  close (logId) {
    if (!this.terminals[logId]) {
      return
    }
    this.terminals[logId].dispose()
    this.terminals[logId] = undefined
  }

  dispose () {
    for (let logId in this.terminals) {
      this.terminals[logId].dispose()
    }
    this.terminals = {}
    super.dispose()
  }

  async stop (...logIds) {
    for (const logId of logIds) {
      if (this.terminals[logId]) {
        await this.terminals[logId].kill()
      }
    }
  }

  async stopAll () {
    const logIds = Object.keys(this.terminals)
    for (const logId of logIds) {
      await this.terminals[logId].kill()
    }
  }
}

module.exports = TerminalChannelManager