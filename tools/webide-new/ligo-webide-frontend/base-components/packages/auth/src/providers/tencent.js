// import { TencentClientId } from '../../config.json'
// const project = process.env.PROJECT
// const serverUrl = process.env.REACT_APP_SERVER_URL
const clientId = process.env.REACT_APP_TENCENT_CLIENT_ID// || TencentClientId

// TODO redirectUri should be modified to REACT_APP_SERVER_URL
// const redirectUri = `${serverUrl}/api/v1/auth/callback/${project}/tencent`
const redirectUri = 'http://tbaas.obsidians.io/callback?provider=tencent'

export default {
  login () {
    window.location.href = this.loginUrl
  },
  get loginUrl() {
    return `https://www.qcloud.com/open/authorize?scope=login&app_id=${clientId}&redirect_url=${redirectUri}`
  }
}
