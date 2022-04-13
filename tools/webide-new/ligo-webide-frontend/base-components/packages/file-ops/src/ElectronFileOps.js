// TODO: re integrate file ops with workspace
import { IpcChannel } from '@obsidians/ipc'

import FileOps from './FileOps'

export default class ElectronFileOps extends FileOps {
  constructor () {
    const fs = window.require('fs-extra')
    const path = window.require('path')
    super(fs, path)

    this.channel = new IpcChannel('file-ops')
    this.electron = window.require('electron')
    this.trash = window.require('trash')

    this.channel.invoke('getPaths').then(result => {
      this.homePath = result.homePath
      this.appPath = result.appPath
      this.workspace = result.workspace
      this.ensureDirectory(this.workspace)
    })
  }

  onFocus (handler) {
    this.electron.ipcRenderer.on('on-focus', handler)
  }

  offFocus (handler) {
    this.electron.ipcRenderer.on('off-focus', handler)
  }

  async openNewFile (defaultPath = this.workspace) {
    const result = await this.channel.invoke('showOpenDialog', {
      properties: ['openFile'],
      defaultPath: this.path.isAbsolute(defaultPath) ? defaultPath : this.path.join(this.workspace, defaultPath),
      filters: [
        // { name: 'all', extensions: ['cpp', 'hpp', 'wasm', 'abi', 'md', 'js', 'json', 'c', 'h', 'o'] }
      ]
    })

    if (result && result.filePaths && result.filePaths[0]) {
      const filePath = result.filePaths[0]
      return { key: filePath, path: filePath }
    } else {
      throw new Error()
    }
  }

  async chooseFolder (defaultPath = this.workspace) {
    const result = await this.channel.invoke('showOpenDialog', {
      buttonLabel: 'Open',
      defaultPath: this.path.isAbsolute(defaultPath) ? defaultPath : this.path.join(this.workspace, defaultPath),
      properties: ['openDirectory', 'createDirectory']
    })

    if (result && result.filePaths && result.filePaths[0]) {
      const filePath = result.filePaths[0]
      return filePath
    } else {
      throw new Error()
    }
  }

  async listFolder (folderPath) {
    const children = await this.fs.readdir(folderPath)
    return await Promise.all(children.map(async name => {
      const childPath = this.path.join(folderPath, name)
      const type = await this.isDirectory(childPath) ? 'folder' : 'file'
      return { type, name, path: childPath }
    }))
  }

  showOpenDialog (options) {
    return this.channel.invoke('showOpenDialog', options)
  }

  async showMessageBox ({ message, buttons }) {
    return await this.channel.invoke('showMessageBox', { message, buttons })
  }

  async openItem (filePath) {
    const exsist = !!(await this.fs.promises.stat(filePath).catch(() => false))

    if(!exsist) {
      const { response } = await this.channel.invoke('showMessageBox', {
        message: `The path '${filePath}' does not exist on this computer.`,
        buttons: ['Remove', 'Cancel']
      })

      if (response === 0) {
        return {
          exsist
        }
      }
    } else {
      const result = await this.channel.invoke('openItem', filePath)
      if (result) {
        throw new Error(`Cannot open <b>${filePath}</b>. Please make sure it exists.`)
      }
    }
  }

  async getAppVersion () {
    return await this.channel.invoke('getAppVersion')
  }

  async showItemInFolder (filePath) {
    const exsist = !!(await this.fs.promises.stat(filePath).catch(() => false))

    if(!exsist) {
      const result = await this.channel.invoke('showMessageBox', {
        message: `The path '${filePath}' does not exist on this computer.`,
        buttons: ['Remove', 'Cancel']
      })
      if (result.response === 0) {
        return {
          exsist
        }
      }
    } else {
      return await this.channel.invoke('showItemInFolder', filePath)
    }
  }

  async openLink (href) {
    return await this.channel.invoke('openLink', href)
  }

  async openInTerminal (filePath) {
    const exsist = !!(await this.fs.promises.stat(filePath).catch(() => false))

    if(!exsist) {
      const result = await this.channel.invoke('showMessageBox', {
        message: `The path '${filePath}' does not exist on this computer.`,
        buttons: ['Remove', 'Cancel']
      })
      if (result.response === 0) {
        return {
          exsist
        }
      }
    } else {
      return await this.channel.invoke('openInTerminal', filePath)
    }
  }

  async deleteFile (filePath) {
    return await this.trash([filePath])
  }
}
