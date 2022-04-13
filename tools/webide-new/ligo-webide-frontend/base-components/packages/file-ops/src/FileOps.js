export default class FileOps {
  constructor (fs, path) {
    this.fs = fs
    this.path = path
  }

  async isDirectory (dirPath) {
    try {
      return (await this.fs.promises.stat(dirPath)).isDirectory()
    } catch (e) {
      return false
    }
  }

  async getDirectory (pathUrl) {
    try {
      return (await this.fs.promises.stat(pathUrl)).isDirectory() ? pathUrl : this.path.dirname(pathUrl)
    } catch (e) {
      return null
    }
  }

  async isFile (filePath) {
    try {
      return (await this.fs.promises.stat(filePath)).isFile()
    } catch (e) {
      return false
    }
  }

  async ensureDirectory (dirPath) {
    try {
      await this.fs.ensureDir(dirPath)
      return true
    } catch (e) {
      console.warn(e)
      return false
    }
  }

  async ensureFile (filePath) {
    try {
      await this.fs.ensureFile(filePath)
      return true
    } catch (e) {
      console.warn(e)
      return false
    }
  }

  async readFile (filePath, encoding = 'utf8') {
    return this.fs.promises.readFile(filePath, { encoding })
  }

  async writeFile (filePath, content) {
    return this.fs.promises.writeFile(filePath, content)
  }

  getDockerMountPath (mountPath) {
    if (process.env.OS_IS_WINDOWS) {
      const { root } = this.path.parse(mountPath)
      const pathRoot = root.split(':')[0].toLowerCase()
      const pathWithoutRoot = mountPath.split(':').pop().split('\\').join('/')
      return `/${pathRoot}${pathWithoutRoot}`
    }
    return mountPath
  }
}
