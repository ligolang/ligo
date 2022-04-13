const fs = require('fs')
const path = require('path')
const chokidar = require('chokidar')
const debounce = require('lodash/debounce')

const debounceByKey = (func, wait) => {
  const cache = new Map()
  return (...args) => {
    if (!cache.has(args[0])) {
      cache.set(args[0], debounce(func, wait))
    }
    return cache.get(args[0])(...args)
  }
}

class FileTreeWatcher {
  constructor (rootDir, client) {
    this.watcher = this.watchDirectory(rootDir)
    this.client = client

    this.debouncedRefreshDirectory = debounceByKey(dir => client.loadAndRefreshDirectory(dir), 200)
    this.debouncedFileModified = debounceByKey(filePath => this.refreshFile(filePath), 200)
    this.debouncedFileDeleted = debounceByKey(filePath => this.deleteFile(filePath), 200)
  }

  dispose () {
    this.watcher.close()
  }

  watchDirectory (rootDir) {
    const watcher = chokidar.watch(rootDir, {
      ignored: p => p.includes('node_modules'),
      ignoreInitial: true
    })
    watcher
      .on('add', path => this.onFileCreated({ path }))
      .on('change', path => this.onFileModified({ path }))
      .on('unlink', path => this.onFileMoved({ path }))
      .on('addDir', path => this.onDirectoryCreated({ path }))
      .on('unlinkDir', path => this.onDirectoryMoved({ path }))

    return watcher
  }

  onFileCreated (details) {
    const { dir } = path.parse(details.path)
    this.debouncedRefreshDirectory(dir)
  }

  onFileModified (details) {
    this.debouncedFileModified(details.path)
  }

  onFileMoved (details) {
    const { dir } = path.parse(details.path)
    this.debouncedFileDeleted(details.path)
    this.debouncedRefreshDirectory(dir)
  }

  onDirectoryCreated (details) {
    const { dir } = path.parse(details.path)
    this.debouncedRefreshDirectory(dir)
  }

  onDirectoryMoved (details) {
    const { dir } = path.parse(details.path)
    this.debouncedRefreshDirectory(dir)
  }

  async refreshFile (filePath) {
    const content = await fs.promises.readFile(filePath, 'utf8')
    if (!content) {
      return
    }
    this.client.channel.send('refresh-file', { path: filePath, content })
  }

  async deleteFile (filePath) {
    this.client.channel.send('delete-file', { path: filePath })
  }
}

module.exports = FileTreeWatcher
