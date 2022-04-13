import ElectronFileOps from './ElectronFileOps'
import WebFileOps from './WebFileOps'

class FileOpsManager {
  constructor () {
    this._fsType = null
    this._fileOps
    this.web = new WebFileOps()
  }

  set fsType (fsType) {
    if (fsType === 'electron') {
      this._fsType = fsType
      this._fileOps = new ElectronFileOps()
      return
    } else if (fsType === 'web') {
      this._fsType = fsType
      this._fileOps = this.web
      return
    }
    throw new Error(`Unknown fsType "${this.fsType}".`)
  }
  get fsType () {
    return this._fsType
  }

  get current () {
    if (!this._fsType) {
      throw new Error('this.fsType is not defined.')
    }
    if (!this._fileOps) {
      throw new Error(`Unknown fsType "${this.fsType}".`)
    }
    return this._fileOps
  }
}

export default new FileOpsManager()
