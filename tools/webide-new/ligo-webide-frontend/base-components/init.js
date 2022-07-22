const fs = require('fs');
const process = require('process');
const child_process = require('child_process');

(async () => {
  const packages = await fs.promises.readdir(`${__dirname}/packages`);
  packages.forEach(package => {
    console.log(`\nBuilding *${package}*`)
    const packagePath = `${__dirname}/packages/${package}`
    const scripts = require(`${packagePath}/package.json`).scripts
    if (scripts && scripts.build) {
      process.chdir(packagePath)
      child_process.execSync('yarn build')
    } else {
      console.log('\nPackage does not need to build.')
    }
    console.log('\n==================================')
  })
})();
