const { ipcMain, net } = require('electron')
const qs = require('qs')

const ipc = require('./ipc')
const ChildProcess = require('./ChildProcess')

class IpcChannel {
  constructor(channel = 'default', uid = '') {
    this.ipc = ipc
    this.channel = channel
    this.uid = uid
    this.cp = new ChildProcess()
    this.callbacks = new Map()
    this.start()
  }

  get channelName() {
    if (this.uid) {
      return `obsidians-ipc-${this.channel}-${this.uid}`
    } else {
      return `obsidians-ipc-${this.channel}`
    }
  }

  get channelResponse() {
    if (this.uid) {
      return `obsidians-ipc-response-${this.channel}-${this.uid}`
    } else {
      return `obsidians-ipc-response-${this.channel}`
    }
  }

  start () {
    if (!this.channel) {
      throw new Error(`Not a valid IpcChannel (channel name: ${this.channel})`)
    }
    ipcMain.handle(this.channelName, (_, method, args) => this.onRequest(method, args))
  }

  dispose () {
    ipcMain.removeHandler(this.channelName)
  }

  async onRequest (method, args) {
    if (!this[method]) {
      throw new Error(`Method ${method} is not defined for channel ${this.channel}`)
    }
    try {
      const result = await this[method](...args)
      return { result }
    } catch (e) {
      console.warn(e)
      return { error: e.message }
    }
  }

  send (method, ...data) {
    this.ipc.send(this.channelResponse, method, ...data)
  }

  async call (req) {
    this.send(req.method, req.id, ...req.params)
    const callback = {}
    const promise = new Promise((resolve, reject) => {
      callback.resolve = resolve
      callback.reject = reject
    })
    this.callbacks.set(req.id, callback)
    return promise
  }
  
  async callback (id, error, result) {
    const callback = this.callbacks.get(id)
    if (callback) {
      if (error) {
        callback.reject(new Error(error))
      } else {
        callback.resolve(result)
      }
      this.callbacks.delete(id)
    }
  }

  async exec (command, config) {
    if (!command.trim()) {
      return
    }
    return await this.cp.exec(command.trim(), config)
  }

  async fetch (url, params, method = 'GET') {
    let body
    if (method === 'GET') {
      if (params) {
        url = url + `?` + qs.stringify(params)
      }
    } else {
      body = JSON.stringify(params)
    }
    
    return await new Promise((resolve, reject) => {
      const request = net.request({ url, method })
      request.setHeader('Content-Type', 'application/json;charset=UTF-8')
      request.on('error', err => {
        if (err.message === 'net::ERR_INTERNET_DISCONNECTED') {
          reject(new Error('Internet Disconnected'))
        }
        reject(err)
      })
      request.on('response', (response) => {
        let body = ''
        response.on('data', chunk => {
          body += chunk
        })
        response.on('end', () => resolve(body))
      })
      if (body) {
        request.write(body)
      }
      request.end()
    })
  }
}

module.exports = IpcChannel