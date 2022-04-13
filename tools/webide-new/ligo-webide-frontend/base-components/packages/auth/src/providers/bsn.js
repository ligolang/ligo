import AWS from 'aws-sdk/global'
import STS from "aws-sdk/clients/sts"
import fileOps from '@obsidians/file-ops'
import platform from '@obsidians/platform'
import decode from 'jwt-decode'
import { BuildService, IpcChannel } from '@obsidians/ipc'

import BaseProvider from './base'

class BsnProvider extends BaseProvider {
  constructor () {
    super('bsn')
    AWS.config.update({ region: this.awsConfig.region })
  }

  get channel () {
    if (!this._channel) {
      const name = platform.isDesktop ? 'bsn' : 'auth'
      this._channel = new IpcChannel(name)
    }
    return this._channel
  }

  get providerUrl () {
    return process.env.REACT_APP_OAUTH_BSN_PROVIDER_URL
  }

  get clientId () {
    return process.env.REACT_APP_OAUTH_BSN_CLIENT_ID
  }

  get redirectUri () {
    return `${process.env.REACT_APP_OAUTH_BSN_REDIRECT_URI}?provider=bsn`
  }

  get loginUrl () {
    return `${this.providerUrl}?scope=all&client_id=${this.clientId}&response_type=code&redirect_uri=${this.redirectUri}`
  }

  get project () {
    return process.env.PROJECT
  }

  get awsConfig () {
    return {
      region: process.env.REACT_APP_AWS_REGION,
      roleArn: process.env.REACT_APP_AWS_ROLE_ARN,
      roleSessionName: process.env.REACT_APP_AWS_ROLE_SESSION_NAME,
    }
  }

  async request () {
    if (platform.isDesktop) {
      const code = await this.channel.invoke('request', {
        loginUrl: this.loginUrl,
        filterUrl: process.env.REACT_APP_OAUTH_BSN_REDIRECT_URI
      })
      return code
    } else {
      window.location.href = this.loginUrl
    }
  }

  async grant (code) {
    if (platform.isDesktop) {
      const { token, profile } = await this.channel.invoke('grant', {
        code,
        redirectUrl: this.redirectUri
      })
      const credentials = { token }

      return { credentials, profile }
    } else {
      const tokens = await this.fetchTokens(code)
      if (!tokens) {
        return {}
      }
      const { token, awsToken } = tokens

      const awsCredential = await this.fetchAwsCredential(awsToken)
      if (!awsCredential) {
        return {}
      }

      const credentials = { token, awsCredential }
      const profile = decode(token)

      return { credentials, profile }
    }
  }

  async logout () {
    if (platform.isDesktop) {
      await this.channel.invoke('logout')
      await new Promise(resolve => setTimeout(resolve, 1000))
      location.reload()
      return
    }
    try {
      await fetch(`${this.serverUrl}/api/v1/auth/logout`, {
        method: 'POST',
        credentials: 'include',
      })
    } catch (error) {}
  }

  async done () {
    if (platform.isDesktop) {
      await this.channel.invoke('close')
      location.reload()
    }
  }

  async update (credentials) {
    if (platform.isDesktop) {
      if (credentials && credentials.token) {
        await this.channel.invoke('updateToken', {
          token: credentials.token
        })
      }
    } else {
      if (credentials && credentials.awsCredential) {
        fileOps.web.fs.updateCredential(credentials.awsCredential)
        BuildService.updateCredential(credentials.awsCredential)
      }
    }
  }

  async restore (profile) {
    if (platform.isDesktop) {
      await this.channel.invoke('updateLogin', { isLogin: !!(profile && profile.username) })
    }
  }

  async fetchTokens (code) {
    try {
      let url
      let method
      let body
      if (code) {
        url = `${this.serverUrl}/api/v1/auth/login`
        method = 'POST'
        body = JSON.stringify({
          code,
          provider: this.name,
          project: this.project,
          redirectUri: this.redirectUri,
        })
      } else {
        url = `${this.serverUrl}/api/v1/auth/refresh-token`
        method = 'GET'
      }

      const response = await fetch(url, {
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        credentials: 'include',
        method,
        body,
      })

      const { token, awsToken } = await response.json()
      return { token, awsToken }
    } catch (error) {
      return
    }
  }

  async fetchAwsCredential (token) {
    const sts = new STS()
    const params = {
      WebIdentityToken: token,
      RoleArn: this.awsConfig.roleArn,
      RoleSessionName: this.awsConfig.roleSessionName,
      DurationSeconds: 3600,
    }
    try {
      const credential = await new Promise((resolve, reject) => {
        sts.assumeRoleWithWebIdentity(params, (err, data) => err ? reject(err) : resolve(data))
      })
      return credential
    } catch (error) {
      return
    }
  }

  handleError({ status, modal }) {
    console.log(status, modal)
    if (!modal) {
      return
    }

    if (status === 403) {
      modal.openModal()
    }
  }

  handleState(state, profile) {
    let path = '/'

    try {
      const {
        urlCode = '',
        appTypeId = '',
        projectId = '',
        orgCode = '',
        appTypeFrameName = '',
        chainCodeId = '',
      } = JSON.parse(atob(decodeURIComponent(state)))
      path = `${profile.username}?urlCode=${urlCode}&networkId=${appTypeId}&projectId=${projectId}&organizationId=${orgCode}&networkName=${appTypeFrameName}&chaincodeId=${chainCodeId}`
    } catch (error) {
      path = '/'
    }

    return {
      path
    }
  }
}

export default BsnProvider
