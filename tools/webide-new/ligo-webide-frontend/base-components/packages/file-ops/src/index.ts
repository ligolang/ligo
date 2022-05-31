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
  workspace: string
  pathHelper: any

  constructor () {
    this.localFs = new IndexedLocalFs()
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
      return { type: 'folder', title: name, key: dirPath, children: [], isLeaf: false, name, path: dirPath, loading: true, remote: true }
    })

    const files: FileInfo[] = Object.keys(dirFiles).filter(item => !dirFiles[item].isDirectory).map(item => {
      const filePath = item
      const name = filePath.replace(`${path}/`, '')
      return { type: 'file', title: name, key: filePath, name, path: filePath, remote: true, isLeaf: true }
    })

    return [...folders, ...files]
  }

  async readFile (path: string, cb: (error?: any, content?: string) => void) {
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

  // async copyFolderToJsonInternal (path, visitFile, visitFolder) {
  //   try {
  //     return await this.localFs.folderToJson(path, visitFile, visitFolder)
  //   } catch (e) {
  //     console.log(e)
  //     return {}
  //   }
  // }

}

export default new FileManager()

export { fileSystems, fileSystem } from './filesystems/fileSystem'
export { indexedDBFileSystem } from './filesystems/indexedDB'
