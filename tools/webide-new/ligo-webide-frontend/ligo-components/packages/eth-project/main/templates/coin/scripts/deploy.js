async function main() {
  const Coin = await ethers.getContractFactory('Coin')
  const deployed = await Coin.deploy()
  console.log('Contract deployed to:', deployed.address)
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error)
    process.exit(1)
  })
