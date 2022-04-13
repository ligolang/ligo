const GLDToken = artifacts.require('GLDToken')

module.exports = async function (deployer, network, accounts) {
  await deployer.deploy(GLDToken, '1000000000')
}
