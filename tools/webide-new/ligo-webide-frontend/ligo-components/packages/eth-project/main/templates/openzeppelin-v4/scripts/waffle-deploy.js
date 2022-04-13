const { ethers } = require('ethers')
const GameItems = require('../build/contracts/GameItems.json')

const provider = ethers.getDefaultProvider('http://localhost:62743')
const signer = provider.getSigner()

async function main() {
  const factory = new ethers.ContractFactory(GameItems.abi, GameItems.bytecode, signer)
  const deployed = await factory.deploy()
  console.log('Contract deployed to:', deployed.address)
}

main()
