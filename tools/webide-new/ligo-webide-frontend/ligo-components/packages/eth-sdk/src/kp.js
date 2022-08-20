import { ethers } from 'ethers'
import { IpcChannel } from '@obsidians/ipc'
import utils from './utils'

const channel = new IpcChannel('keypair')

export default {
  async newKeypair (_, secretType) {
    const secret = await channel.invoke('post', 'new-secret')
    if (secretType === 'mnemonic') {
      const extraEntropy = utils.format.bytesFromHex(secret)
      const wallet = ethers.Wallet.createRandom({ extraEntropy })
      return {
        address: wallet.address.toLowerCase(),
        secret: wallet.mnemonic.phrase,
        secretName: 'Mnemonic',
      }
    } else {
      const address = ethers.utils.computeAddress(secret)
      return {
        address: address.toLowerCase(),
        secret,
        secretName: 'Private Key',
      }
    }
  },
  importKeypair (secret) {
    if (secret.startsWith('0x') || /^[0-9a-zA-Z]{64}$/.test(secret)) {
      if (!secret.startsWith('0x')) {
        secret = '0x' + secret
      }
      const address = ethers.utils.computeAddress(secret)
      return {
        address: address.toLowerCase(),
        secret,
        secretName: 'Private Key',
      }
    } else {
      const wallet = ethers.Wallet.fromMnemonic(secret)
      return {
        address: wallet.address.toLowerCase(),
        secret: wallet.mnemonic.phrase,
        secretName: 'Mnemonic',
      }
    }
  },
  walletFrom (secret) {
    if (secret.startsWith('0x')) {
      return new ethers.Wallet(secret)
    } else {
      return ethers.Wallet.fromMnemonic(secret)
    }
  }
}