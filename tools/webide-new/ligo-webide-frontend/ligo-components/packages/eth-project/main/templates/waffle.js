const config = require('./config.json')

module.exports = {
  sourceDirectory: './contracts',
  outputDirectory: './build/contracts',
  compilerType: 'solcjs',
  compilerVersion: config.compilers.solc,
  compilerOptions: {
    evmVersion: config.compilers.evmVersion,
    optimizer: config.compilers.optimizer,
  },
}