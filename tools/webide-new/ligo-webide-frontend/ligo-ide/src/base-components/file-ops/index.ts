import GistFs, { GistData } from './GistFs'
import pathHelper from 'path-browserify'
import IndexedLocalFs from './IndexedLocalFs'

export type FolderInfo = { 
  type: 'folder',
  title: string, 
  key: string, 
  children: any, // TODO add proper type
  isLeaf: boolean, 
  name: string, 
  path: string, 
  loading: boolean, 
  remote: boolean 
}

export type FileInfo = { 
  type: 'file', 
  title: string, 
  key: string, 
  name: string, 
  path: string, 
  remote: boolean, 
  isLeaf: boolean 
}


class FileManager {
  localFs: IndexedLocalFs
  gistFs: GistFs
  workspace: string
  pathHelper: any

  constructor () {
    this.localFs = new IndexedLocalFs()
    this.gistFs = new GistFs()
    this.workspace = ''  // TODO use ./workspaces as workspace root instead of full path
    this.pathHelper = pathHelper
  }

  async isDirectory (path: string): Promise<boolean> {
    return await this.localFs.stat(path).isDirectory().catch(e => {
      throw new Error(`Fail to check directory: <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async isFile (path: string): Promise<boolean> {
    return await this.localFs.stat(path).isFile().catch(e => {
      throw new Error(`Fail to check file: <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async exists (path: string): Promise<boolean> {
    return await this.localFs.exists(path).catch(e => {
      throw new Error(`Fail to check path: <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async readDirectory (path: string): Promise<(FolderInfo | FileInfo)[]> {
    if (!this.isDirectory(path)) {
      throw new Error(`<b>${path}</b> is not directory.`)
    }

    const dirFiles: {[a: string]: {isDirectory: boolean}} = await this.localFs.readDirectory(path).catch(e => {
      throw new Error(`Fail to fetch directory: <b>${JSON.stringify(e)}</b>.`)
    })

    const folders: FolderInfo[] = Object.keys(dirFiles).filter(item => dirFiles[item].isDirectory).map(item => {
      const dirPath = item
      const name = dirPath.replace(`${path}/`, '')
      return { type: 'folder', title: name, key: dirPath, children: [], isLeaf: false, name, path: dirPath, fatherPath: path, loading: true, remote: true }
    })

    const files: FileInfo[] = Object.keys(dirFiles).filter(item => !dirFiles[item].isDirectory).map(item => {
      const filePath = item
      const name = filePath.replace(`${path}/`, '')
      return { type: 'file', title: name, key: filePath, name, path: filePath, fatherPath: path, remote: true, isLeaf: true }
    })

    return [...folders, ...files]
  }

  async readFile (path: string, cb?: (error?: any, content?: string) => void) {
    return await this.localFs.readFile(path).then(content => {
      if (cb) {
        cb(undefined, content)
      }
      return content
    }).catch(e => {
      if (cb) {
        cb(e, undefined)
      }
      throw new Error(`Fail to fetch file: <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async writeDirectory (path: string) {
    await this.localFs.writeDirectory(path).catch(e => {
      throw new Error(`Fail to create the folder <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async writeFile (path: string, content: string) {
    await this.localFs.writeFile(path, content).catch(e => {
      throw new Error(`Fail to create the file <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async deleteDirectory (path: string) {
    if (await this.exists(path)) {
      if (await this.isFile(path)) {
        await this.deleteFile(path)
      } else {
        const folderElements = await this.readDirectory(path)
        if (folderElements.length === 0) {
          await this.localFs.deleteDirectory(path).catch(e => {
            throw new Error(`Fail to delete the folder <b>${JSON.stringify(e)}</b>.`)
          })
        } else {
          for (const element of folderElements) {
            if (element.type === 'file') {
              await this.deleteFile(element.key)
            } else {
              await this.deleteDirectory(element.key)
            }
          }
          await this.localFs.deleteDirectory(path).catch(e => {
            throw new Error(`Fail to delete the folder <b>${JSON.stringify(e)}</b>.`)
          })
        }
      }
    } else {
      throw new Error(`No such directory <b>${path}</b>.`)
    }
  }

  async deleteFile (path: string) {
    if (await this.exists(path) && await this.isFile(path)) {
      await this.localFs.deleteFile(path).catch(e => {
        throw new Error(`Fail to delete the file <b>${JSON.stringify(e)}</b>.`)
      })
    }
  }

  async relativeDirectory (path: string) {
    return await this.isDirectory(path) ? path : pathHelper.dirname(path).catch(e => {
      throw new Error(`Fail to fetch relative direcroty <b>${JSON.stringify(e)}</b>.`)
    })
  }

  async rename (oldPath: string, newPath: string) {
    if (await this.exists(oldPath)) {
      await this.localFs.rename(oldPath, newPath).catch(e => {
        throw new Error(`Fail to rename: <b>${JSON.stringify(e)}</b>.`)
      })
    }
  }

  async copyMoveFile (oldPath: string, newPath: string, mode: 'copy' | 'move') {
    if (!await this.exists(oldPath)) {
      throw new Error(`No such file: ${oldPath}.`)
    }

    if (await this.exists(newPath)) {
      throw new Error(`File already exists: "${newPath}".`)
    }

    const fileContent = await this.readFile(oldPath)
    await this.writeFile(newPath, fileContent)

    if (mode === 'move') {
      await this.deleteFile(oldPath)
    }
  }

  async copyMoveFolder (oldPath: string, newPath: string, mode: 'copy' | 'move') {
    if (!await this.exists(oldPath)) {
      throw new Error(`No such directory: "${oldPath}".`)
    }

    if (await this.exists(newPath)) {
      throw new Error(`Directory already exists: "${oldPath}".`)
    }

    if (newPath.startsWith(oldPath)) {
      throw new Error(`"${newPath}" is subdirectory of "${oldPath}".`)
    }

    await this.writeDirectory(newPath)

    const folderContent = await this.collectFiles(oldPath)
    for (const file of folderContent) {
      await this.writeFile(newPath + file.path.substring(oldPath.length, file.path.length), file.content)
    }

    if (mode === 'move') {
      await this.deleteDirectory(oldPath)
    }
  }

  async loadGistProject (gistId: string) {
    const data = await this.gistFs.loadData(gistId).then((dt: GistData) => {
      if (!dt.files) {
        throw new Error(dt.message)
      } else {
        return dt.files
      }
    }).catch(e => {
      throw new Error(`<b>${e.message}</b>`)
    })

    const obj: {[a: string]: string} = {}
    Object.keys(data).forEach(element => {
      const path = element.replace(/\.\.\./g, '/')
      obj[path] = data[element]
    })
    return obj
  }

  async uploadGistProject(token: string, projectRoot: string): Promise<string> {
    if (await this.isFile(projectRoot)) {
      throw Error(`${projectRoot} is not a directory`)
    }

    const packaged = await this.copyFolderToJson(projectRoot)
    const packagedObject: {[a: string]: {content: string}} = {}
    packaged.forEach(({ path, content }) => {
      packagedObject[path] = {content: content}
    })
    const description = 'Description'
    return await this.gistFs.uploadData(packagedObject, description, token)
  }

  async copyFolderToJson (path: string): Promise<{path: string, content: string}[]> {
    const files = await this.collectFiles(path)
    const regex = new RegExp(path)
    return files.map(({path: filePath, content}) => {
      const gistFilePath = filePath.replace(regex, '').replace(/\//g, '...')
      const gistFileContent = /^\s+$/.test(content) || !content.length ? '// this line is added to create a gist. Empty file is not allowed.' : content
      return {path: gistFilePath, content: gistFileContent}
    })
  }

  async collectFiles (path: string): Promise<{path: string, content: string}[]> {
    const files: {path: string, content: string}[] = []

    if (await this.exists(path)) {
      const items = await this.readDirectory(path)
      if (items.length !== 0) {
        for (const item of items) {
          const curPath = item.key
          if (await this.isDirectory(curPath)) {
            const folderContent = await this.collectFiles(curPath)
            files.push(...folderContent)
          } else {
            const fileContent = await this.readFile(curPath)
            files.push({path: curPath, content: fileContent})
          }
        }
      }
    }

    return files
  }

  showMessageBox ({ message }) {  // TODO add proper type or remove
    const result = window.confirm(message)
    return { response: result ? 0 : 1 }
  }

  getAppVersion (): string | undefined {
    return process.env.APP_VERSION
  }

  openLink (href) {  // TODO add proper type or remove
    window.open(href, '_blank')
  }
}

export default new FileManager()

export { fileSystems, fileSystem } from './filesystems/fileSystem'
export { indexedDBFileSystem } from './filesystems/indexedDB'
