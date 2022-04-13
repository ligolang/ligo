const { app, dialog } = require('electron')
const stripAnsi = require('strip-ansi')
const cp = require('child_process')

class ChildProcess {
  exec (cmd, config = {}) {
    let proc
    return new Promise((resolve, reject) => {
      proc = cp.spawn(cmd, [], {
        shell: process.platform === 'win32' ? 'powershell.exe' : true,
        ...config,
        env: { ...process.env, ...config.env },
      })

      let logs = ''
      proc.stdout.on('data', data => {
        logs += data
      })

      proc.stderr.on('data', data => {
        resolve({ code: -1, logs: data.toString() })
      })

      proc.on('close', code => {
        this.promise = null
        resolve({ code, logs: stripAnsi(logs) })
      })

      proc.on('error', err => {
        if (err.message.indexOf('powershell') > -1) {
          dialog.showErrorBox('PowerShell is not installed', `You need to have PowerShell properly installed to run ${process.env.PROJECT_NAME || 'this application'}.`)
          app.quit()
        }
        reject(err)
      })
    })
  }
}

module.exports = ChildProcess