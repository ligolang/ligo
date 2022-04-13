import path from 'path-browserify'
import FileOps from './FileOps'
import AwsS3Fs from './AwsS3Fs'

export default class WebFileOps extends FileOps {
  constructor () {
    const fs = new AwsS3Fs()
    super(fs, path)

    this.electron = {}

    this.homePath = '/'
    this.workspace = path.join(this.homePath, process.env.PROJECT_NAME)
  }

  onFocus (handler) {
    // this.electron.ipcRenderer.on('on-focus', handler)
  }

  offFocus (handler) {
    // this.electron.ipcRenderer.on('off-focus', handler)
  }

  async openNewFile (defaultPath = this.workspace) {
    // TODO
  }

  async chooseFolder (defaultPath = this.workspace) {
    // TODO
  }

  async listFolder (folderPath) {
    return await this.fs.list(folderPath)
  }

  showMessageBox ({ message, buttons }) {
    const result = window.confirm(message)
    return { response: result ? 0 : 1 }
  }

  openItem (filePath) {}

  showItemInFolder (filePath) {}

  async createNewFolder (folderPath) {
    try {
      await this.fs.ensureDir(folderPath)
    } catch (e) {
      throw new Error(`Fail to create the folder <b>${folderPath}</b>.`)
    }
  }

  getAppVersion () {
    return process.env.APP_VERSION
  }

  openLink (href) {
    window.open(href, '_blank')
  }

  openInTerminal (filePath) {
  }

  deleteFile (filePath) {
    return this.fs.deleteFile(filePath)
  }
}
