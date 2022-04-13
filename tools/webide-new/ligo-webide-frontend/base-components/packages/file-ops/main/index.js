const { IpcChannel } = require('@obsidians/ipc')
const { app, dialog, shell, BrowserWindow } = require('electron')
const path = require('path')
const os = require('os')

class FileOpsChannel extends IpcChannel {
  constructor () {
    super('file-ops')

    this.homePath = app.getPath('home')
    this.appPath = app.getAppPath()
    this.workspace = path.join(this.homePath, process.env.PROJECT_NAME)
  }

  async getPaths () {
    return {
      homePath: this.homePath,
      appPath: this.appPath,
      workspace: this.workspace
    }
  }

  async showOpenDialog (options) {
    return dialog.showOpenDialog(BrowserWindow.getFocusedWindow(), options)
  }

  showMessageBox ({ message, buttons }) {
    return dialog.showMessageBox({ message, buttons })
  }

  getAppVersion () {
    return app.getVersion()
  }

  async openItem (filePath) {
    return await shell.openPath(filePath)
  }

  showItemInFolder (filePath) {
    return shell.showItemInFolder(filePath)
  }

  openLink (href) {
    return shell.openExternal(href)
  }

  openInTerminal (filePath) {
    if (os.type() === 'Darwin') {
      this.exec(`open -a Terminal "${filePath}"`)
    } else if (os.type() === 'Windows_NT') {
      this.exec(`invoke-expression 'cmd /c start powershell -NoExit -Command { Set-Location "${filePath}" }'`)
    } else {
      this.exec(`gnome-terminal --working-directory=${filePath}`)
    }
  }
}

module.exports = FileOpsChannel
