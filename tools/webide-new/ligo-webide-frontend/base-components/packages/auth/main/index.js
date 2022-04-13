const { IpcChannel } = require('@obsidians/ipc')
const { BrowserWindow } = require('electron')
const path = require('path')
const isDev = require('electron-is-dev')

class AuthManager extends IpcChannel {
  constructor() {
    super('auth')
    this.win = null
  }

  async request({ loginUrl, filterUrl }) {
    const callbackUrl = await new Promise((resolve) => {
      this.createWindow(resolve)
      this.win.loadURL(loginUrl)

      const { session: { webRequest } } = this.win.webContents
      const filter = {
        urls: [`${filterUrl}*`]
      }

      webRequest.onBeforeRequest(filter, ({ url }, callback) => {
        if (url.startsWith(`${filterUrl}/callback?error`)) {
          callback({ cancel: true })
          resolve()
          this.loading()
        } else if (url.startsWith(`${filterUrl}/callback`)) {
          callback({ cancel: true })
          resolve(url)
          this.loading()
        } else {
          callback({ cancel: false })
          resolve()
        }
      })
    })

    if (!callbackUrl) {
      return
    }

    // extrat code from callbackUrl
    const location = new URL(callbackUrl)
    const query = new URLSearchParams(location.search)
    const code = query.get('code')
    return code
  }

  createWindow(resolve) {
    this.win = new BrowserWindow({
      width: 420,
      height: 605,
      alwaysOnTop: true,
      resizable: false,
      backgroundColor: '#2F2F32',
    })

    this.win.on('closed', () => {
      this.win = null
      resolve()
    })
  }

  async close() {
    if (this.win) {
      this.win.close()
    }
  }

  async loading() {
    const redirectURL = isDev ? 'http://localhost:3000/loading.html' : `file://${path.join(__dirname, '../loading.html')}`
    this.win.loadURL(redirectURL)
  }
}

module.exports = AuthManager
