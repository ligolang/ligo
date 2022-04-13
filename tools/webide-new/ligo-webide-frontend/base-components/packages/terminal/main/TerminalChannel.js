const { IpcChannel } = require('@obsidians/ipc')
const Pty = require('@obsidians/pty')

class TerminalChannel extends IpcChannel {
  constructor (uid, cwd) {
    super('terminal', uid)
    this.pty = new Pty(this, cwd)
    this.stopCommand = undefined
  }

  async run (command, config) {
    if (!command.trim()) {
      return
    }
    if (config.stopCommand) {
      this.stopCommand = config.stopCommand
    }
    return await this.pty.run(command.trim(), config)
  }

  write (command) {
    this.pty.write(command)
  }

  resize ({ cols, rows }) {
    this.pty.resize({ cols, rows })
  }

  async kill () {
    return new Promise(resolve => {
      setTimeout(resolve, 10000)
      if (this.stopCommand) {
        this.exec(this.stopCommand).then(resolve)
      } else {
        this.pty.kill().then(resolve)
      }
    })
  }
}
module.exports = TerminalChannel