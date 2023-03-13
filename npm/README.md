# To package
Run,

1. Run `opam exec -- dune build ./src/bin/js_main.bc.js` in the project root.
2. npm run build
3. npm pack

The tarball is ready to be published. `npm run build` will first try to copy `js_main.bc.js` from dune build area to current directory and run the JS bundler to prepare the npm package.
