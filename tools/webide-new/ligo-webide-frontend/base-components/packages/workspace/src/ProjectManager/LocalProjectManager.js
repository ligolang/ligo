import fileOps from '@obsidians/file-ops'
import notification from '@obsidians/notification'
import { modelSessionManager } from '@obsidians/code-editor'

import BaseProjectManager from './BaseProjectManager'
import { sortFile } from './helper'
import { getExamples } from './examples'

export default class LocalProjectManager extends BaseProjectManager {
  static async createProject(name, template) {
    return await this.processProject(name, undefined, template)
  }

  static async openProject(obj, name) {
    return await this.processProject(name, obj, undefined)
  }

  static async processProject(name, obj, template) {
    const data = {
      id: name,
      author: 'local',
      path: '.workspaces/' + name,
      name: name
    }

    if (await fileOps.exists(data.path)) {
      throw new Error('workspace already exists')
    } else {
      await fileOps.writeDirectory(data.path)
    }

    if (obj) {
      for (const key of Object.keys(obj)) {
        try {
          await fileOps.writeFile(data.path + '/' + key, obj[key].content)
        } catch (error) {
          console.error(error)
        }
      }
    } else if (template) {
      const examples = getExamples(data.name, template)

      for (const file in examples) {
        try {
          await fileOps.writeFile(examples[file].name, examples[file].content)
        } catch (error) {
          console.error(error)
        }
      }
    }

    return data
  }

  get path() {
    return fileOps.pathHelper
  }

  async prepareProject() {
    if (!await fileOps.isDirectory(this.projectRoot)) {
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
    return this.projectRoot ? fileOps.pathHelper.join(this.projectRoot, relativePath) : ''
  }

  pathInProject(filePath) {
    return fileOps.pathHelper.relative(this.projectRoot, filePath)
  }

  async readDirectory(folderPath) {
    return await fileOps.readDirectory(folderPath)
  }

  async loadRootDirectory() {
    const result = await this.readDirectory(this.projectRoot)

    const rawData = result.map(item => ({ ...item, pathInProject: `${this.projectName}/${item.name}` }))
    return {
      name: this.projectRoot,
      root: true,
      key: this.projectRoot,
      title: this.projectRoot,
      path: this.projectRoot,
      pathInProject: this.projectRoot,
      loading: false,
      children: sortFile(rawData)
    }
  }

  async loadDirectory(node) {
    const result = await this.readDirectory(node.path)
    const rawData = result.map(item => ({
      ...item,
      pathInProject: this.pathInProject(item.path)
    }))
    return sortFile(rawData)
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
      return await fileOps.isFile(this.mainFilePath)
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
    return await fileOps.isFile(filePath)
  }

  async ensureFile(filePath) {
    return await fileOps.fs.ensureFile(filePath)
  }

  async readFile(filePath, cb) {
    return await fileOps.readFile(filePath, cb)
  }

  async saveFile(filePath, content) {
    await fileOps.writeFile(filePath, content)
  }

  async copyFolderToJson (directory, visitFile, visitFolder) {
    visitFile = visitFile || function () { }
    visitFolder = visitFolder || function () { }
    const regex = new RegExp(directory, 'g')
    await fileOps.copyFolderToJsonInternal(directory, ({ path, content }) => {
      visitFile({ path: path.replace(regex, ''), content })
    }, ({ path }) => {
      visitFolder({ path: path.replace(regex, '') })
    })
  }

  async createNewFile(basePath, name) {
    const filePath = fileOps.pathHelper.join(basePath, name)
    if (await fileOps.exists(filePath)) {
      throw new Error(`File <b>${filePath}</b> already exists.`)
    }

    try {
      await fileOps.writeFile(filePath, '')
    } catch (e) {
      throw new Error(`Fail to create the file <b>${JSON.stringify(e)}</b>.`)
    }

    await this.refreshDirectory(basePath)
    return filePath
  }

  async writeDirectory(basePath, name) {
    const folderPath = fileOps.pathHelper.join(basePath, name)
    if (await fileOps.exists(folderPath)) {
      throw new Error(`Folder <b>${folderPath}</b> already exists.`)
    }

    try {
      await fileOps.writeDirectory(folderPath)
    } catch (e) {
      console.log(JSON.stringify(e))
      throw new Error(`File <b>${JSON.stringify(e)}</b> already exists.`)
    }

    await this.refreshDirectory(basePath)
  }

  // TODO turn on drag events

  // async moveOps({ from, to }) {
  //   const { path, fs } = fileOps
  //   const toDir = await this.getDir(to)
  //   const fromIsFile = await this.isFile(from)
  //   const { name: fromName, ext: fromExt } = path.parse(from)
  //   const dest = fromIsFile ? `${toDir}/${fromName}${fromExt}` : `${toDir}/${fromName}`

  //   const exsist = await await fileOps.exists(path)

  //   try {
  //     if (exsist) {
  //       const { response } = await fileOps.showMessageBox({
  //         message: `A file or folder with the name '${fromName}' already exists. Do you want to replace it?`,
  //         buttons: ['Replace', 'Cancel']
  //       })
  //       if (response === 0) {
  //         await fs.move(from, dest, { overwrite: true })
  //       }
  //     } else {
  //       await fs.move(from, dest)
  //     }
  //   } catch (e) {
  //     throw new Error(`Fail to move <b>${dest}</b>.`)
  //   }
  // }

  // TODO turn on drag events

  // async copyOps({ from, to }) {
  //   const { path } = fileOps
  //   const toDir = await this.getDir(to)
  //   const fromIsFile = await this.isFile(from)
  //   const { name: fromName, ext: fromExt } = path.parse(from)
  //   let dest = !fromIsFile ? `${toDir}/${fromName}` : `${toDir}/${fromName}_copy1${fromExt}`
  //   let safeCount = 0

  //   while (!await this.copy(from, dest) && safeCount < 10) {
  //     const matched = dest.match(/(?<=copy)\d*(?=\.)/g)
  //     safeCount++
  //     if (matched) {
  //       dest = dest.replace(/(?<=copy)\d*(?=\.)/g, Number(matched[0]) + 1)
  //     }
  //   }
  // }

  async rename(oldPath, name) {
    const path = fileOps.pathHelper
    const { dir } = path.parse(oldPath)
    const newPath = path.join(dir, name)

    try {
      await fileOps.rename(oldPath, newPath)
      modelSessionManager.updateEditorAfterMovedFile(oldPath, newPath)
    } catch (e) {
      console.log(e)
      throw new Error(`Fail to rename <b>${oldPath}</b>.`)
    }

    await this.refreshDirectory(dir)
  }

  async deleteFile(node) {
    const { response } = await fileOps.showMessageBox({
      message: `Are you sure you want to delete ${node.path}?`,
      buttons: ['Move to Trash', 'Cancel']
    })
    if (response === 0) {
      if (node.children) {
        await fileOps.deleteDirectory(node.path)
      } else {
        await fileOps.deleteFile(node.path)
      }
    }

    const { dir } = fileOps.pathHelper.parse(node.path)
    await this.refreshDirectory(dir)
  }

  async refreshDirectory (dir) {
    const children = await this.loadDirectory({ path: dir })
    BaseProjectManager.channel.trigger('refresh-directory', { path: dir, children })
  }

  toggleTerminal(terminal) {
    BaseProjectManager.terminalButton?.setState({ terminal })
    this.project.toggleTerminal(terminal)
  }
}
