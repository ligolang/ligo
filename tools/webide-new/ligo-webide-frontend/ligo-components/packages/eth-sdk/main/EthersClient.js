const { ethers } = require('ethers')

module.exports = class EthersClient {
  constructor ({ networkId = '', url }) {
    if (url) {
      this.provider = ethers.getDefaultProvider(url)
    } else {
      this.provider = new ethers.providers.InfuraProvider(networkId, {
        projectId: process.env.INFURA_PROJECT_ID
      })
    }
  }

  async rpc (method, params) {
    return await this.provider.send(method, params)
  }

  async sign (tx, secret) {
    const wallet = this.walletFrom(secret)
    tx.type = tx.type ? parseInt(tx.type) : 0
    tx.from = wallet.address
    tx.nonce = await this.rpc('eth_getTransactionCount', [tx.from, 'latest'])
    // tx.gasLimit = tx.gas
    delete tx.gas
    delete tx.maxFeePerGas
    delete tx.maxPriorityFeePerGas
    const populated = await wallet.populateTransaction(tx)
    return await wallet.signTransaction(populated)
  }

  walletFrom (secret) {
    let wallet
    if (secret.startsWith('0x')) {
      wallet = new ethers.Wallet(secret)
    } else {
      wallet = ethers.Wallet.fromMnemonic(secret)
    }
    return wallet.connect(this.provider)
  }

  async sendRawTransaction (tx) {
    return this.rpc('eth_sendRawTransaction', [tx])
  }
}