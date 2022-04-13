const { ipcRenderer } = window.require('electron')

export default class ElectronIpcChannel {
  constructor(channel = 'default', uid = '') {
    this.channel = channel
    this.uid = uid
    this.listeners = {}
    this._onDataReceived = this._onDataReceived.bind(this)
    ipcRenderer.on(this.channelResponse, this._onDataReceived)
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

  dispose () {
    this.listeners = {}
    ipcRenderer.removeListener(this.channelResponse, this._onDataReceived)
  }

  async invoke (method, ...args) {
    const { result, error } = await ipcRenderer.invoke(this.channelName, method, args)
    if (error) {
      throw new Error(error)
    }
    return result
  }

  on (event, callback) {
    if (!this.listeners[event]) {
      this.listeners[event] = []
    }
    this.listeners[event].push(callback)

    return () => this.off(event, callback)
  }

  off (event, callback) {
    if (!this.listeners[event]) {
      return
    }
    if (!callback) {
      this.listeners[event] = []
      return
    }
    let cbs = this.listeners[event]
    for (let i = 0; i < cbs.length; i++) {
      if (cbs[i] === callback) {
        cbs.splice(i, 1)
        return
      }
    }
  }

  get events () {
    return Object.keys(this.listeners)
  }

  trigger (event, ...args) {
    for (let evt of this.events) {
      if (evt === event || event.startsWith(`${evt}:`)) {
        this.listeners[evt].forEach(cb => cb(...args))
      }
    }
  }

  _onDataReceived (_, method, ...args) {
    this.trigger(method, ...args)
  }
}