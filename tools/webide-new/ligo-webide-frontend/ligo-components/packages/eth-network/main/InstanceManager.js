const fs = require('fs')
const path = require('path')
const os = require('os')

const { IpcChannel } = require('@obsidians/ipc')
const { DockerImageChannel } = require('@obsidians/docker')

class InstanceManager extends IpcChannel {
  constructor () {
    super('node-instance')
    this.dockerChannel = new DockerImageChannel(process.env.DOCKER_IMAGE_NODE)
  }

  async create ({ name, version, networkId = 'dev', miner }) {
    const PROJECT = process.env.PROJECT
    const tmpdir = os.tmpdir()
    const keysFile = path.join(tmpdir, 'keys')
    const pwdFile = path.join(tmpdir, 'pwd')

    await this.exec(`docker volume create --label version=${version},chain=${networkId} ${PROJECT}-${name}`)
    await this.exec(`docker run -di --rm --name ${PROJECT}-config-${name} -v ${PROJECT}-${name}:/data --entrypoint /bin/sh ${process.env.DOCKER_IMAGE_NODE}:${version}`)

    const genesisPath = path.join(__dirname, 'genesis.json')
    await this.exec(`docker cp ${genesisPath} ${PROJECT}-config-${name}:/data`)

    fs.writeFileSync(keysFile, miner.secret.replace('0x', ''))
    fs.writeFileSync(pwdFile, 'password')
    await this.exec(`docker cp ${keysFile} ${PROJECT}-config-${name}:/data`)
    await this.exec(`docker cp ${pwdFile} ${PROJECT}-config-${name}:/data`)
    fs.unlinkSync(keysFile)
    fs.unlinkSync(pwdFile)

    await this.exec(`docker exec ${PROJECT}-config-${name} geth --datadir /data --nousb account import /data/keys --password /data/pwd`)
    // await this.exec(`docker exec ${PROJECT}-config-${name} geth --datadir /data --nousb init /data/genesis.json`)

    await this.exec(`docker stop ${PROJECT}-config-${name}`)
  }

  async list (networkId = 'dev') {
    const { logs: volumes } = await this.exec(`docker volume ls --format "{{json . }}"`)
    const { logs: runnings } = await this.exec(`docker ps --format "{{json . }}"`)
    const instances = volumes.split('\n').filter(Boolean).map(JSON.parse).filter(x => x.Name.startsWith(`${process.env.PROJECT}-`))
    const runningInstances = runnings.split('\n').filter(Boolean).map(JSON.parse).map(x => {
      if (x.Names.startsWith(`${process.env.PROJECT}-`)) {
        return x.Mounts
      }
    }).filter(Boolean)
    const instancesWithLabels = instances.map(i => {
      const labels = {}
      if (runningInstances.indexOf(i.Name) > -1) {
        i.running = true
      }
      i.Labels.split(',').forEach(x => {
        const [name, value] = x.split('=')
        labels[name] = value
      })
      i.Labels = labels
      return i
    })
    return instancesWithLabels.filter(x => x.Labels.chain === networkId)
  }

  async readConfig ({ name, version }) {
    const PROJECT = process.env.PROJECT
    const configPath = path.join(os.tmpdir(), `conflux.toml`)
    try {
      fs.unlinkSync(configPath)
    } catch (e) {}
    await this.exec(`docker run --rm -di --name ${PROJECT}-config-${name} -v ${PROJECT}-${name}:/${PROJECT}-node ${process.env.DOCKER_IMAGE_NODE}:${version} /bin/bash`)
    await this.exec(`docker cp ${PROJECT}-config-${name}:/${PROJECT}-node/default.toml ${configPath}`)
    let config
    try {
      config = fs.readFileSync(configPath, 'utf8')
    } catch (e) {
      return ''
    }
    await this.exec(`docker stop ${PROJECT}-config-${name}`)
    return config
  }

  async writeConfig ({ name, version, content }) {
    const PROJECT = process.env.PROJECT
    const configPath = path.join(os.tmpdir(), 'conflux.toml')
    fs.writeFileSync(configPath, content, 'utf8')
    await this.exec(`docker run --rm -di --name ${PROJECT}-config-${name} -v ${PROJECT}-${name}:/${PROJECT}-node ${process.env.DOCKER_IMAGE_NODE}:${version} /bin/bash`)
    await this.exec(`docker cp ${configPath} ${PROJECT}-config-${name}:/${PROJECT}-node/default.toml`)
    await this.exec(`docker stop ${PROJECT}-config-${name}`)
  }

  async delete (name) {
    await this.exec(`docker volume rm ${process.env.PROJECT}-${name}`)
  }
}

module.exports = InstanceManager