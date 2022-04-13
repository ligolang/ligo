const fastify = require('fastify')({ logger: false })
const { JsonRpcEngine } = require('json-rpc-engine')

module.exports = class RpcServer {
  constructor (Client, options) {
    this.Client = Client
    this.channel = options.channel
    this.keypairManager = options.keypairManager
    this.keypairFilter = options.keypairFilter

    this.client = null
    this.engine = this._startJsonRpcEngine(options.rpcMap)
    this._start(this.engine)
  }

  setNetwork (option) {
    this.client = new this.Client(option)
  }

  unsetNetwork () {
    this.client = null
  }

  _startJsonRpcEngine (rpcMap = {}) {
    const engine = new JsonRpcEngine()
    const middleware = async (req, res, next, end) => {
      if (rpcMap[req.method]) {
        req.method = rpcMap[req.method]
      }
      try {
        if (!this.client) {
          throw new Error('No network is running.')
        } else if (req.method === 'eth_accounts') {
          let keypairs = await this.keypairManager.get()
          if (this.keypairFilter) {
            keypairs = keypairs.filter(keypair => this.keypairFilter(keypair, this.client))
          }
          res.result = keypairs.map(k => k.address)
        } else if (req.method === 'eth_sendTransaction') {
          if (!req.params[0]) {
            throw new Error('Empty transaction data')
          }
          let tx = req.params[0]
          if (!tx.from || !tx.direct) {
            tx = await this.keypairManager.call({ ...req, method: 'signTransaction' })
          }
          const kp = await this.keypairManager.get(tx.from)
          if (!kp) {
            throw new Error(`No keypair for ${tx.from}`)
          }
          // const signed = await this.client.sign(tx, kp.secret)
          delete req.params[0].direct
          const signed = await this.client.sign(req.params[0], kp.secret)
          res.result = await this.client.sendRawTransaction(signed)
        } else {
          res.result = await this.client.rpc(req.method, req.params)
        }
      } catch (err) {
        if (err.body) {
          console.warn(err.body)
          end(err)
        } else {
          this.channel.send('error', err.message)
          end(err)
          return
        }
      }
      
      end()
    }
    engine.push(middleware)
    return engine
  }

  _start (engine, port = 62743) {
    fastify.post('/', function (request, reply) {
      engine.handle(request.body, function (err, response) {
        reply.send(response)
      })
    })

    fastify.listen(port, function (err, address) {
      if (err) {
        fastify.log.error(err)
      }
      // fastify.log.info(`server listening on ${address}`)
    })
  }
}
