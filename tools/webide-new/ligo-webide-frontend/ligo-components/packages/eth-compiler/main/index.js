const { DockerImageChannel } = require('@obsidians/docker')

class CompilerManager {
  constructor () {
    this.truffle = new DockerImageChannel(process.env.DOCKER_IMAGE_COMPILER)
    this.solc = new DockerImageChannel('ethereum/solc')
  }
}

module.exports = CompilerManager