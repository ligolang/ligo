const path = require('path')
const FileHound = require('filehound')

const FileTreeWatcher = require('./FileTreeWatcher')

class FileTreeClient {
  constructor (rootDir, channel) {
    const { dir, base } = path.parse(rootDir)
    this.rootDir = path.join(dir, base)
    this.channel = channel

    this.tree = {
      name: base,
      root: true,
      path: this.rootDir,
      loading: true,
      children: []
    }
    this.treePointer = {
      [this.rootDir]: this.tree
    }

    this.watcher = new FileTreeWatcher(rootDir, this)
  }

  dispose () {
    this.watcher.dispose()
  }

  async ready () {
    return this.loadDirectory(this.rootDir)
  }

  async loadAndRefreshDirectory (directory) {
    const node = await this.loadDirectory(directory)
    this.channel.send('refresh-directory', node)
  }

  async loadDirectory (directory) {
    if (!this.treePointer[directory]) {
      return
    }
    if (this.treePointer[directory].children) {
      this.treePointer[directory].children = []
    }

    await FileHound
      .create()
      .paths(directory)
    // .ignoreHiddenDirectories()
    // .ignoreHiddenFiles()
      .directory()
      .not()
      .glob('node_modules', '.git', 'cache', '.cache')
      .depth(1)
      .find()
      .then(dirs => {
        dirs.forEach(dirPath => {
          const { dir, base } = path.parse(dirPath)
          this.treePointer[dirPath] = this.treePointer[dirPath] || {
            name: base,
            path: dirPath,
            loading: true,
            children: [],
            type: 'folder',
            isLeaf: false,
          }
          this.treePointer[dir].children.push(this.treePointer[dirPath])
        })
      })

    await FileHound
      .create()
      .paths(directory)
    // .ignoreHiddenDirectories()
      .ignoreHiddenFiles()
      .depth(0)
      .find()
      .then(files => {
        files.forEach(filePath => {
          const { dir, base } = path.parse(filePath)
          this.treePointer[dir].children.push({
            name: base,
            path: filePath,
            type: 'file',
            isLeaf: true,
          })
        })
      })

    this.treePointer[directory].loading = false

    return this.treePointer[directory]
  }
}

module.exports = FileTreeClient