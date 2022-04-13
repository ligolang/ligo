import { IpcChannel } from '@obsidians/ipc'
import { DockerImageChannel } from '@obsidians/docker'
import semver from 'semver'

const channel = new IpcChannel('node-instance')

channel.node = new DockerImageChannel(process.env.DOCKER_IMAGE_NODE, {
  filter: v => semver.valid(v)
})

export default channel