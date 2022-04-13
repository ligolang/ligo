const Main = artifacts.require('Main')

module.exports = async function (deployer, network, accounts) {
  await deployer.deploy(Main)
}
