async function main() {
  const GLDToken = await ethers.getContractFactory('GLDToken')
  const deployed = await GLDToken.deploy('1000000000')
  console.log('Contract deployed to:', deployed.address)
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error)
    process.exit(1)
  })
