const { autoUpdater } = require('electron-updater')

const { IpcChannel } = require('@obsidians/ipc')

class AutoUpdate extends IpcChannel {
  constructor (url) {
    super('auto-update')

    autoUpdater.setFeedURL(url)
    autoUpdater.autoInstallOnAppQuit = false
  
    autoUpdater.on('error', info => this.sendStatus('error', info))
    autoUpdater.on('checking-for-update', info => this.sendStatus('checking-for-update', info))
    autoUpdater.on('update-available', info => this.sendStatus('update-available', info))
    autoUpdater.on('update-not-available', info => this.sendStatus('update-not-available', info))
    autoUpdater.on('download-progress', info => this.sendStatus('download-progress', info))
    autoUpdater.on('update-downloaded', info => this.sendStatus('update-downloaded', info))
  }

  sendStatus (type, info) {
    this.send('status', { type, info })
  }

  check () {
    autoUpdater.checkForUpdates()
  }

  updateNow () {
    autoUpdater.quitAndInstall()
  }
}

module.exports = AutoUpdate