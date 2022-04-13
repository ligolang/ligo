import platform from '@obsidians/platform'
import fileOps from '@obsidians/file-ops'

export default class SolcjsCompiler {
  constructor () {
    this.reqs = new Map()
    this.fileCache = new Map()

    this.projectManager = null
  }

  getWebWorker (solcUrl) {
    if (this.solcUrl !== solcUrl) {
      if (this.worker) {
        this.worker.terminate()
        this.worker = undefined
      }
    }
    this.solcUrl = solcUrl
    if (!this.worker) {
      const solcFile = (platform.isWeb || process.env.NODE_ENV === 'development')
        ? '/solc.js'
        : fileOps.current.path.join(fileOps.current.appPath, `build/solc.js`)
      this.worker = new Worker(solcFile)
      this.worker.onmessage = this.onMessage.bind(this)
    }
    return this.worker
  }

  async makeRequest (worker, method, data) {
    const id = Math.floor(Math.random() * 1000)
    const promise = new Promise((resolve, reject) => {
      this.reqs.set(id, { resolve, reject })
    })

    worker.postMessage({ id, method, data })
    return promise
  }

  onMessage (e) {
    const { id, method, data } = e.data

    if (method === 'getFile') {
      this.getFile(data)
    } else if (this.reqs.has(id)) {
      this.reqs.get(id).resolve(data)
    } else {
      console.warn(`[SolcjsCompiler] request ${id} not found.`)
    }
  }

  async compile(solcUrl, projectManager) {
    this.fileCache = new Map()
    this.projectManager = projectManager
    const mainFilePath = projectManager.projectSettings.get('main')

    let mainFileContent
    try {
      mainFileContent = await projectManager.readFile(projectManager.mainFilePath)
    } catch (e) {
      console.warn(e)
      throw new Error(`Cannot read the main file <b>${mainFilePath}</b>.`)
    }

    const evmVersion = projectManager.projectSettings.get('compilers.evmVersion')
    const optimizer = projectManager.projectSettings.get('compilers.optimizer')

    const data = {
      language: 'Solidity',
      sources: {
        [mainFilePath]: { content: mainFileContent }
      },
      settings: {
        outputSelection: { '*': { '*': ['*'] } },
        optimizer: optimizer || { enabled: false },
        evmVersion,
      }
    }

    const worker = this.getWebWorker(solcUrl)

    return this.makeRequest(worker, 'compile', {
      solcUrl,
      input: JSON.stringify(data)
    })
  }

  async getFile ({ path, buffer }) {
    let result
    try {
      if (path.startsWith('http://') || path.startsWith('https://')) {
        const res = await fetch(path)
        result = { contents: await res.text() }
      } else {
        result = { contents: await this.readFile(path) }
      }
    } catch (e) {
      result = { error: e.message }
    }
    const resultJson = JSON.stringify(result)

    let len = resultJson.length
    const contentArray = new Uint16Array(buffer.content)
    if (len * 2 <= contentArray.length) {
      for (let i = 0, len = resultJson.length; i < len; i++) {
        contentArray[i] = resultJson.charCodeAt(i)
      }
    } else {
      len = -resultJson.length * 2
    }

    const lenArray = new Int32Array(buffer.len)
    Atomics.store(lenArray, 0, len)
    Atomics.notify(lenArray, 0, 1)
  }

  async readFile (path) {
    try {
      const completePath = this.projectManager.pathForProjectFile(path)
      if (!this.fileCache.has(completePath)) {
        this.fileCache.set(completePath, await this.projectManager.readFile(completePath))
      }
      return this.fileCache.get(completePath)
    } catch (e) {
      console.warn(`[SolcjsCompiler] readFile error`)
      console.warn(e)
      throw new Error('File not found')
    }
  }
}
