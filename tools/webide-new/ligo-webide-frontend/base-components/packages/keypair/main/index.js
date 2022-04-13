const keytar = require('keytar')
const { randomBytes } = require('crypto')

const { IpcChannel } = require('@obsidians/ipc')

class KeypairManager extends IpcChannel {
  constructor(project) {
    super('keypair')
    this.project = project
  }

  async get (address) {
    if (address) {
      return this.loadSecret(address)
    } else {
      const keys = await keytar.findCredentials(`@obsidians/${this.project}-keypair`)
      return keys.map(({ account }) => ({ address: account }))
    }
  }

  async loadSecret (address) {
    const secret = await keytar.getPassword(`@obsidians/${this.project}-keypair`, address)
    if (secret) {
      return { address, secret }
    }
  }

  async post (task, keypair) {
    if (task === 'new-secret') {
      const privKey = randomBytes(32).toString('hex')
      return `0x${privKey}`
    } else {
      await this.save(keypair.address, keypair.secret)
    }
  }

  async put () {}

  async save (address, secret) {
    await keytar.setPassword(`@obsidians/${this.project}-keypair`, address, secret)
  }

  async delete (address) {
    await keytar.deletePassword(`@obsidians/${this.project}-keypair`, address)
  }
}

module.exports = KeypairManager
