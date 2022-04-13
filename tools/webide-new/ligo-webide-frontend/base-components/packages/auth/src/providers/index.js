import GithubProvider from './github'
import BsnProvider from './bsn'
import tencent from './tencent'
import NoUsere from './nouser'

export default {
  github: new GithubProvider(),
  bsn: new BsnProvider(),
  nouser: new NoUsere(),
  tencent,
}