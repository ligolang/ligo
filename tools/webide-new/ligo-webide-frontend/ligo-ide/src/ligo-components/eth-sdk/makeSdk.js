import { IpcChannel } from '~/base-components/ipc'
import notification from '~/base-components/notification'

let current
const channel = new IpcChannel('sdk')
channel.off('error')
channel.on('error', msg => {
  if (current) {
    current.dismiss()
  }
  current = notification.error('Error', msg)
})

export default function makeSdk ({
  kp,
  networks,
  customNetworks = [],
  utils,
  rpc,
  namedContracts = {},
  Client,
  Contract,
  TxManager,
  BrowserExtension,
}) {
  let browserExtension

  return class Sdk {
    static get kp () { return kp }
    static get networks () { return networks }
    static get customNetworks () { return customNetworks }

    static InitBrowserExtension (networkManager) {
      browserExtension = BrowserExtension && BrowserExtension.Init(networkManager)
    }

    constructor ({ id, ...option }) {
      this.client = new Client({ networkId: id, ...option })
      this.networkId = id
      this.txManager = new TxManager(this.client)
    }

    dispose () {
      this.client.dispose()
    }

    get url () { return this.client.url }
    get chainId () { return this.client.chainId }

    get utils () { return utils }
    get rpc () { return rpc }
    get namedContracts () { return namedContracts }

    isValidAddress (address) {
      return utils.isValidAddress(address, this.chainId)
    }

    async networkInfo () {
      return await this.client.networkInfo()
    }

    async getStatus () {
      return await this.client.getStatus()
    }

    async latest () {
      return await this.client.latest()
    }

    async accountFrom (address) {
      return await this.client.getAccount(address)
    }

    contractFrom ({ address, abi }) {
      return new Contract({ address, abi }, this.client)
    }

    async getTransferTransaction (...args) {
      return await this.txManager.getTransferTx(Contract, ...args)
    }

    async getDeployTransaction (...args) {
      return await this.txManager.getDeployTx(...args)
    }

    async estimate (arg) {
      return await this.txManager.estimate(arg)
    }

    sendTransaction (arg) {
      return this.txManager.sendTransaction(arg, browserExtension)
    }

    async getTransactions (address, page = 0, size = 10) {
      return await this.client.getTransactions(address, page, size)
    }

    async getTokens (address) {
      return await this.client.getTokens(address)
    }

    async getTokenInfo (address) {
      return await this.client.getTokenInfo(address)
    }

    async callRpc (method, parameters) {
      const params = rpc.prepare(parameters, false, this)
      return await this.client.callRpc(method, params)
    }
  }
}