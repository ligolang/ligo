import platform from '@obsidians/platform'
import { IpcChannel } from '@obsidians/ipc'
import decode from 'jwt-decode'

class BaseProvider {
  constructor(name) {
    this.name = name
    this.profile = null
    this._channel = null
  }

  get channel () {
    if (!this._channel) {
      this._channel = new IpcChannel('auth')
    }
    return this._channel
  }

  get serverUrl () {
    return process.env.REACT_APP_OAUTH_SERVER_URL
  }

  get providerUrl () {
    return ''
  }

  get clientId () {
    return ''
  }

  get redirectUri () {
    return ''
  }

  get loginUrl () {
    return ''
  }

  async request () {
    if (platform.isDesktop) {
      const code = await this.channel.invoke('request', {
        loginUrl: this.loginUrl,
        filterUrl: `${this.serverUrl}/api/v1/auth`
      })
      return code
    } else {
      window.location.href = this.loginUrl
    }
  }

  async grant (code) {
    return { credentials: {}, profile: '' }
  }

  async update (credentials) {
    return
  }

  async restore (profile) {
    return
  }

  shouldRefresh (token) {
    // Check token expiration
    try {
      const { exp } = decode(token)
      const currentTs = Math.floor(Date.now() / 1000)
      return exp - currentTs < 60
    } catch (error) {
      return true
    }
  }

  async done () {
    // Close window for desktop
    if (platform.isDesktop) {
      await this.channel.invoke('close')
    }
  }

  async logout () {
    // Clear token
  }

  handleError ({ status, modal }) {
    // Handle Error
  }

  handleState(state, profile) {
    return {
      path: '/'
    }
  }
}

export default BaseProvider