import { IpcChannel } from '~/base-components/ipc'
import { DockerImageChannel } from '~/base-components/docker'
import semver from 'semver'

const channel = new IpcChannel('node-instance')

channel.node = new DockerImageChannel(process.env.DOCKER_IMAGE_NODE, {
  filter: v => semver.valid(v)
})

export default channel