const pty = require('node-pty')
const defaultShell = require('default-shell')

class Pty {
  constructor (ipcChannel, cwd) {
    this.ipcChannel = ipcChannel
    this.cwd = cwd
    this.promise = null
    this.config = {}
  }

  createProc (cmd, config, { resolve, reject }) {
    let switches
    let shell = defaultShell
    if (process.platform === 'win32') {
      shell = 'powershell.exe'
      switches = ['-NoLogo', '-NoProfile', '-Command', cmd]
    } else if (defaultShell.indexOf('zsh') > -1) {
      switches = ['--no-globalrcs', '--no-rcs', '-i', '-c', cmd]
    } else {
      shell = '/bin/bash'
      switches = ['--noprofile', '--norc', '-i', '-c', cmd]
    }

    const proc = pty.spawn(shell, switches, {
      name: this.ipcChannel.channelName,
      cols: this.cols || 87,
      rows: this.rows || 1,
      useConpty: false,
      cwd: this.cwd,
      ...config,
      env: { ...process.env, ...config.env },
    })

    let logs = ''
    proc.onData(data => {
      logs += data
      this.ipcChannel.send('data', data)
      if (config.resolveOnFirstLog) {
        resolve({ logs })
      }
      if (config.resolveOnLog) {
        if (config.resolveOnLog.test(logs)) {
          resolve({ logs })
        }
      }
    })

    proc.onExit(e => {
      this.proc = null
      if (config.returnCodeOnly ) {
        resolve({ code: e.exitCode })
      } else {
        resolve({ code: e.exitCode, logs })
      }
    })

    return proc
  }

  run (cmd, config = {}) {
    if (this.proc) {
      throw new Error('Pty is already running a process.')
    }

    return new Promise((resolve, reject) => {
      this.proc = this.createProc(cmd, config, { resolve, reject })
    })
  }

  write (cmd) {
    if (this.proc) {
      this.proc.write(cmd)
    }
  }

  resize ({ cols, rows }) {
    this.cols = cols
    this.rows = rows
    if (this.proc) {
      this.proc.resize(cols, rows)
    }
  }

  kill (signal) {
    if (this.proc) {
      this.proc.write(Uint8Array.from([0x03, 0x0d])) // send ctrl+c
      this.proc.destroy()
      // reject(new Error(signal))
      // return this.promise.catch(e => true)
    }
    return Promise.resolve()
  }
}

module.exports = Pty