export default class IndexedLocalFs {
  stat (path: string) {
    return {
      isDirectory: async (): Promise<boolean> => (await window.ligoIdeFileSystem.stat(path)).isDirectory(),
      isFile: async (): Promise<boolean> => (await window.ligoIdeFileSystem.stat(path)).isFile()
    }
  }

  async exists (path: string): Promise<boolean> {
    return await window.ligoIdeFileSystem.exists(path)
  }

  async readDirectory(path: string): Promise<{[a: string]: {isDirectory: boolean}}> {
    const files = await window.ligoIdeFileSystem.readdir(path)
    const trailPath = path.replace(/^\/|\/$/g, '')
    const result: {[a: string]: {isDirectory: boolean}} = {}
    if (files) {
      for (let file of files) {
        const trailFile = file.replace(/^\/|\/$/g, '')
        const absPath = trailPath + '/' + trailFile
        result[absPath] = { isDirectory: await this.stat(absPath).isDirectory() }
      }
    }
    return result
  }

  async readFile (filePath: string): Promise<string> {
    return await window.ligoIdeFileSystem.readFile(filePath)
  }

  async writeDirectory (path: string): Promise<void> {
    const paths = path.split('/')
    if (paths.length && paths[0] === '') {
      paths.shift()
    }

    let curDir = ''
    for (const value of paths) {
      curDir = curDir + '/' + value
      if (!await this.exists(curDir)) {
        await window.ligoIdeFileSystem.mkdir(curDir)
      }
    }
  }

  async writeFile (filePath: string, content: string): Promise<void> {
    await this.writeDirectory(filePath.substring(0, filePath.lastIndexOf('/')))
    await window.ligoIdeFileSystem.writeFile(filePath, content)
  }

  async deleteFile (filePath: string): Promise<void> {
    await window.ligoIdeFileSystem.unlink(filePath)
  }

  async deleteDirectory (dirPath: string): Promise<void> {
    await window.ligoIdeFileSystem.rmdir(dirPath)
  }

  async rename (oldPath: string, newPath: string): Promise<void> {
    await window.ligoIdeFileSystem.rename(oldPath, newPath)
  }

  // async folderToJson (path, visitFile, visitFolder) {
  //   visitFile = visitFile || function () { /* do nothing. */ }
  //   visitFolder = visitFolder || function () { /* do nothing. */ }

  //   const json = {}
  //   if (await window.ligoIdeFileSystem.exists(path)) {
  //     try {
  //       const items = await window.ligoIdeFileSystem.readdir(path)
  //       visitFolder({ path })
  //       if (items.length !== 0) {
  //         for (const item of items) {
  //           const file = {}
  //           const curPath = `${path}${path.endsWith('/') ? '' : '/'}${item}`
  //           if ((await window.ligoIdeFileSystem.stat(curPath)).isDirectory()) {
  //             file.children = await this.folderToJson(curPath, visitFile, visitFolder)
  //           } else {
  //             file.content = await window.ligoIdeFileSystem.readFile(curPath, 'utf8')
  //             visitFile({ path: curPath, content: file.content })
  //           }
  //           json[curPath] = file
  //         }
  //       }
  //     } catch (e) {
  //       console.log(e)
  //       throw new Error(e)
  //     }
  //   }
  //   return json
  // }
}
