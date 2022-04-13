import { instanceChannel } from '@obsidians/eth-network'
import compiler from '@obsidians/eth-compiler'
import { dockerChannel } from '@obsidians/docker'

export default async function checkDependencies (extras = []) {
  try {
    const results = await Promise.all([
      dockerChannel.check(),
      instanceChannel.node.installed(),
      compiler.truffle.installed(),
      ...extras.map(item => item.channel.installed()),
    ])
    return results.every(x => !!x)
  } catch (e) {
    return false
  }
}