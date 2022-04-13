const { ethers } = require('ethers')
const ERC20 = require('../build/contracts/ERC20.json')

const provider = ethers.getDefaultProvider('http://localhost:62743')
const signer = provider.getSigner()

async function main() {
  const factory = new ethers.ContractFactory(ERC20.abi, ERC20.bytecode, signer)
  const deployed = await factory.deploy('Test Token', 'TT', '1000000000')
  console.log('Contract deployed to:', deployed.address)
}

main()
