const Coin = artifacts.require('Coin')

module.exports = async function (deployer, network, accounts) {
  await deployer.deploy(Coin)
}
