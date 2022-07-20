import { ethers } from 'ethers'

import platform from '@obsidians/platform'
import { IpcChannel } from '@obsidians/ipc'
import redux from '@obsidians/redux'

import utils from '../utils'
import tokenlist from './tokenlist.json'

export default class EthersClient {
  constructor (option) {
    const { networkId = '', chainId, url } = option
    this.networkId = networkId
    this.chainId = chainId

    if (url) {
      this.provider = ethers.getDefaultProvider(url)
    } else {
      if (window.ethereum) {
        this.provider = new ethers.providers.Web3Provider(window.ethereum, 'any')
        this.provider.isMetaMask = true
      } else {
        this.provider = new ethers.providers.InfuraProvider(networkId, {
          projectId: process.env.INFURA_PROJECT_ID
        })
      }
    }
    
    this.explorer = new ExplorerProxy(networkId)

    if (platform.isDesktop) {
      this.channel = new IpcChannel('sdk')
      this.channel.invoke('setNetwork', option)
    } else {
      this.channel = new IpcChannel()
    }
  }

  get url () {
    return this.provider && this.provider.connection && this.provider.connection.url
  }

  dispose () {
    if (platform.isDesktop) {
      this.channel.invoke('unsetNetwork')
    }
  }

  async networkInfo () {
    return await this.provider.getNetwork()
  }

  async getStatus () {
    return await this.provider.getBlock('latest')
  }

  async latest () {
    const status = await this.getStatus()
    return status.number
  }

  async getAccount (address) {
    const balance = await this.provider.getBalance(address)
    const code = await this.provider.getCode(address)
    const nonce = await this.provider.getTransactionCount(address)
    const codeHash = ethers.utils.keccak256(code)
    return {
      address,
      balance: utils.unit.fromValue(balance),
      nonce: BigInt(nonce).toString(10),
      codeHash: codeHash === '0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470' ? null : codeHash,
    }
  }

  async getTransactions (address, page, size) {
    address = address.toLowerCase()
    if (this.networkId.startsWith('dev')) {
      const { queue } = redux.getState()
      const txs = queue.getIn([this.networkId, 'txs'])
      if (!txs) {
        return { length: 0, list: [] }
      }

      const filtered = txs.filter(tx => {
        const from = tx.getIn(['data', 'transaction', 'from']) || ''
        const to = tx.getIn(['data', 'transaction', 'to']) || ''
        return tx.get('status') === 'CONFIRMED' &&
          (address === from.toLowerCase() || address === to.toLowerCase())
      })

      const list = filtered.map(tx => ({
        ...tx.getIn(['data', 'transaction']).toJS(),
        ...tx.getIn(['data', 'receipt']).toJS(),
        timeStamp: tx.get('ts'),
        method: tx.getIn(['data', 'functionName']),
      })).toArray()

      return { length: list.length, list }
    }

    const result = await this.explorer.getHistory(address, page, size)
    return { length: 0, list: result.result }
  }

  async getTokens (address) {
    if (this.chainId !== 1) {
      return
    }
    const url = `https://services.tokenview.com/vipapi/eth/address/tokenbalance/${address}?apikey=${process.env.TOKENVIEW_API_TOKEN}`
    let json
    try {
      const result = await this.channel.invoke('fetch', url)
      json = JSON.parse(result)
    } catch {
      return
    }
    if (json.code !== 1) {
      return
    }
    return json.data.map(t => {
      const token = tokenlist.tokens.find(token => token.address.toLowerCase() === t.tokenInfo.h)
      return {
        type: 'ERC20',
        balance: t.balance,
        name: t.tokenInfo.f,
        symbol: t.tokenInfo.s,
        decimals: Number(t.tokenInfo.d),
        address: t.tokenInfo.h,
        icon: token && token.logoURI,
      }
    })
  }

  async getTokenInfo (address) {
    if (this.chainId !== 1) {
      return
    }
    const token = tokenlist.tokens.find(t => t.address.toLowerCase() === address)
    if (token) {
      token.icon = token.logoURI
      token.address = token.address.toLowerCase()
      token.totalSupply = await this._getTokenTotalSupply(address)
      return token
    }
  }

  async _getTokenTotalSupply (address) {
    const result = await this.explorer.getTokenTotalSupply(address)
    return result.result
  }

  async callRpc (method, params) {
    return await this.provider.send(method, params)
  }
}


class ExplorerProxy {
  constructor (networkId) {
    this.networkId = networkId
    this.channel = new IpcChannel('explorer')
  }

  async getHistory (address, page = 0, size = 10) {
    const query = {
      module: 'account',
      action: 'txlist',
      address,
      startblock: 0,
      endblock: 99999999,
      page: page + 1,
      offset: size,
      sort: 'desc'
    }
    return await this.channel.invoke('GET', this.networkId, query)
  }

  async getTokenTotalSupply (address) {
    const query = {
      module: 'stats',
      action: 'tokensupply',
      contractaddress: address,
    }
    return await this.channel.invoke('GET', this.networkId, query)
  }
}
