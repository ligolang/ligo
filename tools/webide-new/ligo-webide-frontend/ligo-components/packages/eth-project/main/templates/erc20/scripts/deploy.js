async function main() {
  const ERC20 = await ethers.getContractFactory('ERC20')
  const deployed = await ERC20.deploy('Test Token', 'TT', '1000000000')
  console.log('Contract deployed to:', deployed.address)
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error)
    process.exit(1)
  })
