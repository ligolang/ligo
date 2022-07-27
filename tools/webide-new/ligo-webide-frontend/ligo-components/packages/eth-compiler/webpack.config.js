const path = require('path')

module.exports = {
  mode: 'production',
  entry: './solc/index.js',
  output: {
    filename: 'solc-wrapper.js',
    path: path.resolve(__dirname, 'dist'),
    globalObject: 'self',
  },
  resolve: {
    alias: {
      path: false,
      module: false,
      memorystream: false,
      'follow-redirects': false,
    },
    fallback: {
      assert: require.resolve('assert'),
    }
  }
}