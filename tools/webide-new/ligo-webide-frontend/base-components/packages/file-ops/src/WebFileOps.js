import path from 'path-browserify'
import FileOps from './FileOps'
import AwsS3Fs from './AwsS3Fs'

export default class WebFileOps extends FileOps {
  constructor () {
    const fs = new AwsS3Fs()
    super(fs, path)

    this.homePath = '/'
    this.workspace = path.join(this.homePath, process.env.PROJECT_NAME)
  }

  onFocus (handler) {
  }

  offFocus (handler) {
  }

  async openNewFile (defaultPath = this.workspace) {
  }

  async chooseFolder (defaultPath = this.workspace) {
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
