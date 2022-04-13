import wrapper from 'solc/wrapper'
try {
  global.wrapper = wrapper
} catch (err) {
  console.log("[Webpack-bundle] err ", err)
}