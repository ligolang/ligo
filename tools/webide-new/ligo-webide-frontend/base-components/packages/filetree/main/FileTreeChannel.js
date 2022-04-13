const { IpcChannel } = require('@obsidians/ipc')

const FileTreeClient = require('./FileTreeClient')

class FileTreeChannel extends IpcChannel {
  constructor() {
    super('project')
    this.fileTreeClient = null
  }

  async loadTree (projectRoot) {
    if (this.fileTreeClient) {
      this.fileTreeClient.dispose()
    }

    this.fileTreeClient = new FileTreeClient(projectRoot, this)
    return this.fileTreeClient.ready()
  }

  async loadDirectory(directory) {
    const node = await this.fileTreeClient.loadDirectory(directory)
    return node.children
  }
}

module.exports = FileTreeChannel
