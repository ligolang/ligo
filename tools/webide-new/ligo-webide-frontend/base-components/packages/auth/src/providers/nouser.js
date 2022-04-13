import AWS from 'aws-sdk/global'
import fileOps from '@obsidians/file-ops'
import { BuildService } from '@obsidians/ipc'

import BaseProvider from './base'

class NoUserProvider extends BaseProvider {
  constructor () {
    super('nouser')
    AWS.config.update({ region: this.awsConfig.region })
  }

  get providerUrl () {
    return process.env.REACT_APP_OAUTH_GITHUB_PROVIDER_URL
  }

  get clientId () {
    return process.env.REACT_APP_OAUTH_GITHUB_CLIENT_ID
  }

  get redirectUri () {
    return `${process.env.REACT_APP_OAUTH_GITHUB_REDIRECT_URI}/${this.project}/github`
  }

  get loginUrl () {
    return `${this.providerUrl}?client_id=${this.clientId}&scope=read:user&redirect_uri=${this.redirectUri}`
  }

  get project () {
    if(process.env.REACT_APP_ENV === 'development') {
      return 'eth-test'
    }

    if(process.env.NODE_ENV === 'development') {
      return 'local'
    }

    return process.env.PROJECT
  }

  get awsConfig () {
    return {
      region: process.env.REACT_APP_AWS_REGION,
      roleArn: process.env.REACT_APP_AWS_ROLE_ARN,
      roleSessionName: process.env.REACT_APP_AWS_ROLE_SESSION_NAME,
    }
  }

  // give no user a token to access public aws files
  async getNoUserAuth(){
    // this.credentials = credentials
    // this.update()

    const url = `${this.serverUrl}/api/v1/auth/public-aws-token`
    const response = await fetch(url, {
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      credentials: 'include',
      method: 'GET',
    })
    const { awsToken } = await response.json()
    const awsCredential = await this.fetchAwsCredential(awsToken)
    const credentials = { token: awsToken, awsCredential }
    return credentials
  }

  async update (credentials) {
    if (credentials && credentials.awsCredential) {
      fileOps.web.fs.updateCredential(credentials.awsCredential)
      BuildService.updateCredential(credentials.awsCredential)
    }
  }

  async fetchAwsCredential (token) {
    const sts = new AWS.STS()
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
}

export default NoUserProvider
