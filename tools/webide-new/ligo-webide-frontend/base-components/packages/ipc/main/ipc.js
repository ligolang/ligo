class Ipc {
  constructor() {
    this.window = null
  }

  send(channel, method, ...data) {
    if (this.window && this.window.webContents) {
      this.window.webContents.send(channel, method, ...data)
    }
  }
}

module.exports = new Ipc()