import fileOps from '@obsidians/file-ops'
import notification from '@obsidians/notification'
import { modelSessionManager } from '@obsidians/code-editor'

import BaseProjectManager from './BaseProjectManager'
import { sortFile } from './helper'

export default class LocalProjectManager extends BaseProjectManager {
  static async createProject(options, stage = '') {
    return await BaseProjectManager.channel.invoke('post', stage, options)
  }

  constructor(project, projectRoot) {
    super(project, projectRoot)

    BaseProjectManager.channel.on('refresh-file', this.onRefreshFile.bind(this))
    BaseProjectManager.channel.on('delete-file', this.onDeleteFile.bind(this))
  }

  dispose() {
    BaseProjectManager.channel.off('refresh-file')
    BaseProjectManager.channel.off('delete-file')
  }

  get path() {
    return fileOps.current.path
  }

  async prepareProject() {
    if (!await fileOps.current.isDirectory(this.projectRoot)) {
      return { error: 'invalid project' }
    }

    let projectSettings
    try {
      projectSettings = await this.readProjectSettings()
    } catch (e) {
      console.warn(e)
      return { initial: { path: this.settingsFilePath, pathInProject: this.settingsFilePath }, projectSettings: null }
    }

    if (await this.isMainValid()) {
      return { initial: { path: this.mainFilePath, pathInProject: this.mainFilePath }, projectSettings }
    }
    return { initial: { path: this.settingsFilePath, pathInProject: this.settingsFilePath }, projectSettings }
  }

  pathForProjectFile(relativePath) {
    return this.projectRoot ? fileOps.current.path.join(this.projectRoot, relativePath) : ''
  }

  pathInProject(filePath) {
    return this.path.relative(this.projectRoot, filePath)
  }

  async getDir(filePath) {
    return await fileOps.current.getDirectory(filePath)
  }
  async listFolder(folderPath) {
    return await fileOps.current.listFolder(folderPath)
  }

  async loadRootDirectory() {
    const rootResult = await BaseProjectManager.channel.invoke('loadTree', this.projectRoot)
    const isHasFileREADME = rootResult.children.length === 0 ? false : rootResult.children.find(item => item.name === 'README.md')
    !isHasFileREADME && this.createNewFile(this.projectRoot, 'README.md')
    rootResult.children = sortFile(rootResult.children)
    return rootResult
  }

  async loadDirectory(node) {
    const fileNode = await BaseProjectManager.channel.invoke('loadDirectory', node.path)
    return sortFile(fileNode)
  }

  async readProjectSettings() {
    this.projectSettings = new BaseProjectManager.ProjectSettings(this, this.settingsFilePath, BaseProjectManager.channel)
    await this.projectSettings.readSettings()
    return this.projectSettings
  }

  openProjectSettings() {
    this.project.openProjectSettings(this.settingsFilePath)
  }

  get mainFilePath() {
    if (this.projectSettings?.get('main')) {
      return this.pathForProjectFile(this.projectSettings.get('main'))
    }
    throw new Error('No main file in project settings')
  }

  async isMainValid() {
    try {
      return await fileOps.current.isFile(this.mainFilePath)
    } catch (e) {
      return false
    }
  }

  async checkSettings() {
    if (!this.project || !this.projectRoot) {
      notification.error('No Project', 'Please open a project first.')
      return
    }

    return await this.projectSettings.readSettings()
  }

  async isFile(filePath) {
    return await fileOps.current.isFile(filePath)
  }

  async ensureFile(filePath) {
    return await fileOps.current.fs.ensureFile(filePath)
  }

  async readFile(filePath) {
    return await fileOps.current.readFile(filePath)
  }

  async saveFile(filePath, content) {
    await fileOps.current.writeFile(filePath, content)
  }

  onFileChanged() { }

  async createNewFile(basePath, name) {
    const filePath = fileOps.current.path.join(basePath, name)
    if (await fileOps.current.isFile(filePath)) {
      throw new Error(`File <b>${filePath}</b> already exists.`)
    }

    try {
      await this.ensureFile(filePath)
    } catch (e) {
      if (e.code === 'EISDIR') {
        throw new Error(`Folder <b>${filePath}</b> already exists.`)
      } else {
        throw new Error(`Fail to create the file <b>${filePath}</b>.`)
      }
    }
    return filePath
  }

  async createNewFolder(basePath, name) {
    const folderPath = fileOps.current.path.join(basePath, name)
    if (await fileOps.current.isDirectory(folderPath)) {
      throw new Error(`Folder <b>${folderPath}</b> already exists.`)
    }

    try {
      await fileOps.current.fs.ensureDir(folderPath)
    } catch (e) {
      if (e.code === 'EISDIR') {
        throw new Error(`File <b>${folderPath}</b> already exists.`)
      } else {
        throw new Error(`Fail to create the folder <b>${folderPath}</b>.`)
      }
    }
  }

  async copy(from, to) {
    const { fs } = fileOps.current
    try {
      await fs.copy(from, to, { overwrite: false, errorOnExist: true });
      return true;
    } catch (error) {
      return false
    }
  }
  async checkExsist (path) {
    const { fs } = fileOps.current
    return !!(await fs.promises.stat(path).catch(() => false))
  }

  async moveOps({ from, to }) {
    const { path, fs } = fileOps.current
    const toDir = await this.getDir(to)
    const fromIsFile = await this.isFile(from)
    const { name: fromName, ext: fromExt } = path.parse(from)
    const dest = fromIsFile ? `${toDir}/${fromName}${fromExt}` : `${toDir}/${fromName}`

    const exsist = await this.checkExsist(dest)

    try {
        if (exsist) {
          const { response } = await fileOps.current.showMessageBox({
            message: `A file or folder with the name '${fromName}' already exists. Do you want to replace it?`,
            buttons: ['Replace', 'Cancel']
          })
          if (response === 0) {
            await fs.move(from, dest, { overwrite: true })
          }
        } else {
          await fs.move(from, dest)
        }
    } catch (e) {
      throw new Error(`Fail to move <b>${dest}</b>.`)
    }
  }

  async copyOps({ from, to }) {
    const { path } = fileOps.current
    const toDir = await this.getDir(to)
    const fromIsFile = await this.isFile(from)
    const { name: fromName, ext: fromExt } = path.parse(from)
    let dest = !fromIsFile ? `${toDir}/${fromName}` : `${toDir}/${fromName}_copy1${fromExt}`
    let safeCount = 0

    while (!await this.copy(from, dest) && safeCount < 10) {
      const matched = dest.match(/(?<=copy)\d*(?=\.)/g)
      safeCount++
      if (matched) {
        dest = dest.replace(/(?<=copy)\d*(?=\.)/g, Number(matched[0]) + 1)
      }
    }
  }

  async rename(oldPath, name) {
    const { path, fs } = fileOps.current
    const { dir } = path.parse(oldPath)
    const newPath = path.join(dir, name)

    try {
      await fs.rename(oldPath, newPath)
      modelSessionManager.updateEditorAfterMovedFile(oldPath, newPath)
    } catch (e) {
      throw new Error(`Fail to rename <b>${oldPath}</b>.`)
    }

  }

  async deleteFile(node) {
    const { response } = await fileOps.current.showMessageBox({
      message: `Are you sure you want to delete ${node.path}?`,
      buttons: ['Move to Trash', 'Cancel']
    })
    if (response === 0) {
      await fileOps.current.deleteFile(node.path)
    }
  }

  onRefreshFile(data) {
    modelSessionManager.refreshFile(data)
    if (data.path === this.settingsFilePath) {
      this.projectSettings?.update(data.content)
    }
  }

  onDeleteFile(data) {
    modelSessionManager.deleteFile(data.path)
  }

  toggleTerminal(terminal) {
    BaseProjectManager.terminalButton?.setState({ terminal })
    this.project.toggleTerminal(terminal)
  }
}
