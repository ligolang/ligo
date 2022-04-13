const GameItems = artifacts.require('GameItems')

module.exports = async function (deployer, network, accounts) {
  await deployer.deploy(GameItems)
}
