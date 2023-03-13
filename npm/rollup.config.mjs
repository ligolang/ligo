import commonjs from "@rollup/plugin-commonjs";
import resolve from "@rollup/plugin-node-resolve";
import nodePolyfills from "rollup-plugin-polyfill-node";
import json from "@rollup/plugin-json";
import { importMetaAssets } from "@web/rollup-plugin-import-meta-assets";

export default {
  input: "./ligo.js",
  output: {
    dir: "./dist",
    format: "umd",
    name: "ligo"
  },
  plugins: [
    // importMetaAssets({
    //   include: [
    //     // Uncomment if you need rollup to extract out the assets
    //     // "node_modules/@ligolang/secp256k1-wasm/src/*.js",
    //     // "node_modules/@ligolang/hacl-wasm/**.js",
    //     // "node_modules/@ligolang/ocaml-bls12-381/dist/*.js",
    //     "./*.js",
    //   ],
    // }),
    json(),
    commonjs(),
    // Uncomment if you need to bundle all the JS loaders of wasm dependencies
    // resolve(),
    nodePolyfills(/* options */),
  ],
};
