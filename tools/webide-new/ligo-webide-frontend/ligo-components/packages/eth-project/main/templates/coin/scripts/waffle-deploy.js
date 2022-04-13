const { ethers } = require('ethers')
const Coin = require('../build/contracts/Coin.json')

const provider = ethers.getDefaultProvider('http://localhost:62743')
const signer = provider.getSigner()

async function main() {
  const factory = new ethers.ContractFactory(Coin.abi, Coin.bytecode, signer)
  const deployed = await factory.deploy()
  console.log('Contract deployed to:', deployed.address)
}

main()
