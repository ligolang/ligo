export default class HttpIpcChannel {
  constructor(channel = 'default', uid = '') {
    this.channel = channel
    this.uid = uid
    this.listeners = {}
    this._onDataReceived = this._onDataReceived.bind(this)
  }

  get channelName() {
    if (this.uid) {
      return `${this.channel}-${this.uid}`
    } else {
      return `${this.channel}`
    }
  }

  get channelResponse() {
    if (this.uid) {
      return `response-${this.channel}-${this.uid}`
    } else {
      return `response-${this.channel}`
    }
  }

  dispose () {
    this.listeners = {}
  }

  async invoke (method, ...args) {
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
