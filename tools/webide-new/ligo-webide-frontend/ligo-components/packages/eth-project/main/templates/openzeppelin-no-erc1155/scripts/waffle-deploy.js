const { ethers } = require('ethers')
const GLDToken = require('../build/contracts/GLDToken.json')

const provider = ethers.getDefaultProvider('http://localhost:62743')
const signer = provider.getSigner()

async function main() {
  const factory = new ethers.ContractFactory(GLDToken.abi, GLDToken.bytecode, signer)
  const deployed = await factory.deploy('1000000000')
  console.log('Contract deployed to:', deployed.address)
}

main()
