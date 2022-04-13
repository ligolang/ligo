import Auth from '@obsidians/auth'
import io from 'socket.io-client'

export default class IoIpcChannel {
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

  // get channelResponse() {
  //   if (this.uid) {
  //     return `response-${this.channel}-${this.uid}`
  //   } else {
  //     return `response-${this.channel}`
  //   }
  // }

  get socket () {
    if (!this._socket) {
      this._socket = this.createSocket()
    }
    return this._socket
  }

  async createSocket () {
    const token = await Auth.getToken()
    const { REACT_APP_IPC_SERVER_URL } = process.env
    const socketUrl = `${REACT_APP_IPC_SERVER_URL.replace('http', 'ws')}/${this.channel}`

    return new Promise((resolve, reject) => {
      const socket = io(socketUrl, { query: { token, uid: this.uid } })
      socket.on('connect', () => resolve(socket))
      socket.on('disconnect', () => console.debug(`${this.uid} disconnected.`))
      socket.on('res', this._onDataReceived)
      socket.on('error', msg => {
        console.warn(msg)
        reject(msg)
      })
    })
  }

  async dispose () {
    this.listeners = {}
    if (this._socket) {
      (await this.socket).close()
    }
  }

  async invoke (method, ...args) {
    const socket = await this.socket
    return new Promise(resolve => {
      socket.emit(method, ...args, resolve)
    })
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
    console.log(event, args)
    // for (let evt of this.events) {
    //   if (evt === event || event.startsWith(`${evt}:`)) {
    //     this.listeners[evt].forEach(cb => cb(...args))
    //   }
    // }
  }

  _onDataReceived (method, ...args) {
    this.trigger(method, ...args)
  }
}