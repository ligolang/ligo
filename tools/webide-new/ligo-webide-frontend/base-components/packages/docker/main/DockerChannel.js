const { IpcChannel } = require('@obsidians/ipc')
const os = require('os')

class DockerChannel extends IpcChannel {
  constructor () {
    super('docker')
  }

  async check () {
    const result = await this.exec('docker info')
    return !result.code || result.logs.startsWith('WARNING:')
  }

  async version () {
    const result = await this.exec('docker -v')
    if (result.code) {
      return ''
    }
    return result.logs
  }

  async launch () {
    if (os.type() === 'Darwin') {
      this.exec('open /Applications/Docker.app')
    } else if (os.type() === 'Linux') {
      return false
    } else {
      // Try to start Docker Toolbox
      const toolboxResult = await this.exec('docker-machine start')
      if (toolboxResult.code) {
        // Get Docker Desktop path
        const { logs = ''} = await this.exec('(Get-Command docker).Path')
        const desktopPath = logs.replace('Resources\\bin\\docker.exe', 'Docker Desktop.exe').trim()
        if (!desktopPath.endsWith('Desktop.exe')) {
          return false
        }
        // Try to start Docker Desktop
        const desktopResult = await this.exec(`Start-Process "${desktopPath}"`)
        if (desktopResult.code) {
          return false
        }
      }
    }
    return new Promise(resolve => {
      const delay = 500
      let counter = 5 * 60 * (1000 / delay) // 5 mins
      const h = setInterval(async () => {
        if (--counter <= 0) {
          clearInterval(h)
          resolve(false)
        }
        if (await this.check()) {
          clearInterval(h)
          resolve(true)
        }
      }, delay)
    })
  }
}

module.exports = new DockerChannel()
