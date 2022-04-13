const { ethers } = require('ethers')
const Main = require('../build/contracts/Main.json')

const provider = ethers.getDefaultProvider('http://localhost:62743')
const signer = provider.getSigner()

async function main() {
  const factory = new ethers.ContractFactory(Main.abi, Main.bytecode, signer)
  const deployed = await factory.deploy()
  console.log('Contract deployed to:', deployed.address)
}

main()
