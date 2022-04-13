async function main() {
  const Main = await ethers.getContractFactory('Main')
  const deployed = await Main.deploy()
  console.log('Contract deployed to:', deployed.address)
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error)
    process.exit(1)
  })
